#import "conf.typ": conf, render, nothing

#set document(title: [
  Теория формальных языков \
  Лабораторная работа №3
])

#show: conf.with(
  description: "Вариант №26",
  author: "Чайкин Семён",
)

= Задание

Дан контексто-свободный язык $ {w_1 a^k b^k w_2 | k > 2 "&" w_1 in (a a b b b^*)^* "&" |w_1|_(a a) = |w_2|_(a b)} $

Необходимо:
- Проанализировать на детерминизм, $"LL"$-свойство, беспрефиксность;
- Построить КС-грамматику;
- Построить аппроксимации $L$ сверху, используя $"LR"(0)$- и $"LL"(1)$-КА.

// Pereira-Wright Approximation: https://arxiv.org/pdf/cmp-lg/9603002

= PDA

Построим PDA, распознающий язык $L$:

#figure(
  render("img/DPDA.dot"),
  caption: "DPDA"
) <dpda>

Заметим, что этот PDA является детерминированными, так как все переходы являются однозначными.

Очевидно, что $L != nothing$. Заметим, что $(forall omega)((omega in L) -> (omega a^+ in L))$, значит $L$ не является prefix-free. Следовательно, $L$ не является $"LR"(0)$-КС языком.

= КС-грамматика <grammar>

Для языка $L$ построим следующую /* крайне примитивную */ грамматику:

$
S -> A R'_A \
A -> L A R | a a M b R_B \ // Здесь именно R_b так как иначе не учитывается случай, что w_2 начинается с букв b
M -> a b | a M b \ // Центральный блок a^(k-2) b^(k-2)
L -> a a b R_B \ // Порождает aabbb^*
R -> R_A R_B \ // Порождает подстроки a^+ b^+
R_A -> a R_A | a \ // Естественно это максимальное тупое действие в грамматике, но это нужно чтобы не переделывать полностью LR(0)-аппроксимацию, которую сделал с ошибкой. Т.е., до этого была ошибка, сейчас всё правильно
R_B -> b R_B | b \ // Просто b^+
R'_A -> a R'_A | epsilon   // Просто a^_
$

Альтернативно запишем её в следующем виде:

$
S -> A R'_A \
A -> a a b R_B A a R'_A R_B | a a M b R_B \
M -> a b | a M b \
R_B -> b R_B | b \
R'_A -> a R'_A | epsilon
$

Преобразуем грамматику следующим образом:

$
S -> A R'_A \
A -> a a A' \
A' -> L A R | M b R_B \
M -> a M' \
M' -> b | M b \
L -> b R_B \
R -> R_A R_B \
R_B -> b R'_B \
R'_B -> b R'_B | epsilon \
R_A -> a R'_A \
R'_A -> a R'_A | epsilon
$

Докажем, что последняя грамматика является $"LL"(1)$-грамматикой. Построим $"FIRST"$ и $"FOLLOW"$ множества.

#align(center)[
#grid(
  rows: (auto),
  columns: (auto, auto),
  gutter: 1em,
  align: left,
  $"FIRST"_1(S) = {a}$,             $"FOLLOW"_1(S) = {\$}$,
  $"FIRST"_1(A) = {a}$,             $"FOLLOW"_1(A) = {a}$,
  $"FIRST"_1(A') = {a, b}$,         $"FOLLOW"_1(A') = {a}$,
  $"FIRST"_1(M) = {a}$,             $"FOLLOW"_1(M) = {b}$,
  $"FIRST"_1(M') = {a, b}$,         $"FOLLOW"_1(M') = {b}$,
  $"FIRST"_1(L) = {b}$,             $"FOLLOW"_1(L) = {a}$,
  $"FIRST"_1(R) = {a}$,             $"FOLLOW"_1(R) = {a}$,
  $"FIRST"_1(R_B) = {b}$,           $"FOLLOW"_1(R_B) = {a}$,
  $"FIRST"_1(R'_B) = {epsilon, b}$, $"FOLLOW"_1(R'_B) = {a}$,
  $"FIRST"_1(R_A) = {a}$,           $"FOLLOW"_1(R_A) = {b}$,
  $"FIRST"_1(R'_A) = {a, epsilon}$, $"FOLLOW"_1(R'_A) = {\$, b}$,
)
]

Проверим критерий $"LL"(1)$-грамматики:

#align(center)[
#grid(
  rows: (auto),
  columns: (auto),
  gutter: 1em,
  align: left,
  $A': "FIRST"_1(L) inter "FIRST"_1(M) = {b} inter {a} = nothing$,
  $M': "FIRST"_1(b) inter "FIRST"_1(M) = {b} inter {a} = nothing$,
  $R'_B: "FIRST"_1(b) inter "FOLLOW"_1(R'_B) = {b} inter {a} = nothing$,
  $R'_A: "FIRST"_1(a) inter "FOLLOW"_1(R'_A) = {a} inter {\$, b} = nothing$,
)
]

Таким образом, крайняя грамматика является $"LL"(1)$-грамматикой.

= Аппроксимация

== $"LL"(1)$-аппроксимация

Начнём с $"LL"(1)$-аппроксимации. Для этого линеаризуем получившуюся $"LL"(1)$-грамматику:

$
S -> A R'_A \
A -> a_1 a_2 A' \
A' -> L A R | M b_3 R_B \
M -> a_4 M' \
M' -> b_5 | M b_6 \
L -> b_7 R_B \
R -> R_A R_B \
R_B -> b_8 R'_B \
R'_B -> b_9 R'_B | epsilon \
R_A -> a_10 R'_A \
R'_A -> a_11 R'_A | epsilon
$

Теперь вычислим $"First"$, $"Follow"$ и $"Last"$ множества:

$ "First"(G) = {a_1} $
$ "Last"(G) = {b_8, b_9, a_11} $

$
"Follow"(G) = {& a_1 a_2, a_2 a_4, a_2 b_7, b_3 b_8, \
& a_4 a_4, a_4 b_5, b_5 b_6, b_5 b_3, \
& b_6 b_6, b_6 b_3, b_7 b_8, b_8 b_9, \
& b_8 a_10, b_8 a_11, b_9 b_9, b_9 a_11, \
& a_10 b_8, a_10 a_11, a_11 b_8, a_11, a_11}
$

// $ "First"(G) = "First"(S) = "First"(A) = {a_1} $

// $
// "Last"(G) = "Last"(S) = "Last"(R'_A) union "Last"(A) \
// "Last"(R'_A) = {a_11}\
// "Last"(A) = "Last"(A') = "Last"(R) union "Last"(R_B) = "Last"(R_B)\
// "Last"(R_B) = {b_8, b_9}\
// "Last"(G) = {b_8, b_9, a_11}
// $

// "Follow"(a_1) = {a_2}\
// "Follow"(a_2) = "First"(A') = "First"(L) union "First"(M) = {b_7} union {a_4} = {a_4, b_7}\
// "Follow"(b_3) = "First"(R_B) = {b_8}\
// "Follow"(a_4) = "First"(M') = "First"(M) union {b_5} = {a_4, b_5}\
// // Символы "в конце правил" самые неприятные для такого разбора
// "Follow"(b_5) = {b_6, b_3}\ // a_4 a_4 M' b_6, a_4 M' b_3
// "Follow"(b_6) = {b_6, b_3}\
// "Follow"(b_7) = "First"(R_B) = {b_8}\
// "Follow"(b_8) = {b_9, a_11, a_10}\
// "Follow"(b_9) = {b_9, a_11}\
// "Follow"(a_10) = {a_11, b_8}\
// "Follow"(a_11) = {b_8, a_11}

На их основе получаем следующий НКА:

#figure(
  render("img/LLApprox.dot"),
  caption: [$"LL"(1)$-аппроксимация],
) <llapprox-before-reduce>

Частично минимизированный НКА:

#figure(
  render("img/LLApproxAfterBisim.dot"),
  caption: [$"LL"(1)$-аппроксимация (минимизированный НКА)],
) <llapprox>

== $"LR"(0)$-аппроксимация

// Суета. Нет слов, одни эмоции

#figure(
  render("img/LRChar.dot"),
  caption: [Характеристическая машина грамматики]
) <lrchar>

#figure(
  render("img/LRBeforeFlat.dot"),
  caption: [Промежуточное представление]
)

#figure(
  render("img/LRAfterFlat.dot"),
  caption: [Сплющенный автомат]
)

#figure(
  render("img/LRApprox.dot"),
  caption: [$"LR"(0)$-аппроксимация]
)

// :cool: Аппроксимации точно не самые грубые и точно не совпадают друг с другом...

