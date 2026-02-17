package main

import (
	"fmt"
	"go/ast"
	"go/format"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"os"
)

func main() {
	if len(os.Args) != 2 {
		return
	}

	fset := token.NewFileSet()
	if file, err := parser.ParseFile(fset, os.Args[1], nil, parser.ParseComments); err == nil {
		info := &types.Info{
			Uses: make(map[*ast.Ident]types.Object),
		}

		pkg := file.Name.Name

		conf := types.Config{Importer: importer.Default()}
		if _, err := conf.Check(pkg, fset, []*ast.File{file}, info); err != nil {
			panic(err)
		}

		ast.Inspect(file, func(n ast.Node) bool {
			if _, ok := n.(*ast.Ident); !ok {
				return true
			}

			ident := n.(*ast.Ident)
			if obj, exists := info.Uses[ident]; exists {
				if _, ok := obj.(*types.Const); !ok {
					return true
				}

				c := obj.(*types.Const)
				name := c.Name()

				if name[len(name)-1] != '_' {
					return true
				}

				ident.Name = c.Val().ExactString()
			}

			return true
		})

		if format.Node(os.Stdout, fset, file) != nil {
			fmt.Printf("Formatter error: %v\n", err)
		}
	} else {
		fmt.Printf("Errors in %s\n", os.Args[1])
	}
}
