//
// Copyright (c) 2022 Markku Rossi
//
// All rights reserved.
//

package scm

import (
	"fmt"
	"io"
)

// CompileFile compiles the Scheme file.
func (vm *VM) CompileFile(file string) (Code, error) {
	parser, err := NewParser(file)
	if err != nil {
		return nil, err
	}
	defer parser.Close()

	for {
		v, err := parser.Next()
		if err != nil {
			if err != io.EOF {
				return nil, err
			}
			break
		}

		err = vm.compileValue(v)
		if err != nil {
			return nil, err
		}
	}
	vm.addInstr(OpHalt, nil, 0)

	return vm.compiled, nil
}

func (vm *VM) compileValue(value Value) error {
	var stackDepth int

	switch v := value.(type) {
	case *Cons:
		length, ok := ListLength(v)
		if !ok {
			return fmt.Errorf("compile value: %v", v)
		}

		name, args, body, ok, err := isDefineFunc(v)
		if err != nil {
			return err
		}
		if ok {
			fmt.Printf("(define (%s %v) %v\n", name, args, body)
			break
		}

		// Create a call frame.
		vm.addInstr(OpPushS, nil, length-1)

		// Evaluate arguments.
		li := v.Cdr
		for j := 0; li != nil; j++ {
			cons, ok := li.(*Cons)
			if !ok {
				return fmt.Errorf("invalid list: %v", li)
			}
			err := vm.compileValue(cons.Car)
			if err != nil {
				return err
			}
			li = cons.Cdr
			instr := vm.addInstr(OpLocalSet, nil, stackDepth)
			instr.J = j
		}

		// Compile function.
		err = vm.compileValue(v.Car)
		if err != nil {
			return err
		}

		// Function call. XXX tail-call
		vm.addInstr(OpCall, nil, 0)

		return nil

	case *Identifier:
		// XXX local vs. global
		instr := vm.addInstr(OpGlobal, nil, 0)
		instr.Sym = vm.Intern(v.Name)

	case Keyword:
		return fmt.Errorf("unexpected keyword: %s", v)

	case *Boolean, *String, *Character, Number:
		vm.addInstr(OpConst, v, 0)

	default:
		return fmt.Errorf("compile value: %v(%T)", v, v)
	}
	return nil
}

func (vm *VM) addInstr(op Operand, v Value, i int) *Instr {
	instr := &Instr{
		Op: op,
		V:  v,
		I:  i,
	}
	vm.compiled = append(vm.compiled, instr)
	return instr
}

func isDefineFunc(cons *Cons) (name *Identifier, args []*Identifier, body *Cons,
	ok bool, err error) {

	if !isKeyword(cons.Car, KwDefine) {
		return
	}

	lst, ok := Car(Cdr(cons, true))
	if !ok {
		return
	}
	err = Map(func(v Value) error {
		id, ok := v.(*Identifier)
		if !ok {
			return fmt.Errorf("lambda: arguments must be identifiers")
		}
		if name == nil {
			name = id
		} else {
			args = append(args, id)
		}
		return nil
	}, lst)
	if err != nil {
		return
	}
	if name == nil {
		ok = false
		return
	}

	lst, ok = Cdr(Cdr(cons, true))
	if !ok {
		return
	}
	body, ok = lst.(*Cons)

	return
}

func isKeyword(value Value, keyword Keyword) bool {
	kw, ok := value.(Keyword)
	if !ok {
		return false
	}
	return kw == keyword
}
