#import "@preview/diagraph:0.3.6" as diagraph

#let conf(
  description: none,
  author: none,
  show_toc: true,
  doc
) = {
  set text(12pt, font: "New Computer Modern")

  set page(numbering: none)
  align(center)[
    #v(15em)

    #text(weight: "bold")[#title()]

    #v(0.5cm)

    #text(size: 18pt)[#description]

    #v(1.5cm)

    #text(size: 14pt)[#author]
  ]

  set figure(supplement: "Рисунок")
  
  set page(numbering: "1")
  set heading(numbering: "1.")
  counter(page).update(1)

  if show_toc {
    outline(title: "Содержание")
  }

  pagebreak()

  doc
}

#let render(image) = {
  context diagraph.render(
    read(image),
    width: page.width * 0.75,
    math-mode: "math"
  )
}

#let nothing = $diameter$;
