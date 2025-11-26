//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

// Package html creates (html entities) library.
package html

import (
	"fmt"
	"sort"
)

// Entity defines an HTML entity.
type Entity struct {
	name  string
	value string
}

// Entities creates the Scheme entity vector.
func Entities() {
	entity, entity2 := entityMaps()

	var entities []*Entity
	var max int

	for k, v := range entity {
		if len(k) > max {
			max = len(k)
		}
		entities = append(entities, &Entity{
			name:  k,
			value: fmt.Sprintf("\\x%08X;", v),
		})
	}
	for k, v := range entity2 {
		if len(k) > max {
			max = len(k)
		}
		entities = append(entities, &Entity{
			name:  k,
			value: fmt.Sprintf("\\x%08X;\\x%08X;", v[0], v[1]),
		})
	}
	sort.Slice(entities, func(i, j int) bool {
		return entities[i].name < entities[j].name
	})

	fmt.Printf(`;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(library (html entities (1 0))
  (export html-entities)
  (import (rnrs base))

  (define html-entities
`)
	for idx, e := range entities {
		if idx == 0 {
			fmt.Printf("    '#(")
		} else {
			fmt.Printf("       ")
		}
		fmt.Printf("(%q ", e.name)
		for i := len(e.name); i < max; i++ {
			fmt.Print(" ")
		}
		fmt.Printf(". \"%s\")\n", e.value)
	}
	fmt.Printf("      )\n    )\n  )\n")
}
