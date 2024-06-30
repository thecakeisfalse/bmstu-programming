% TODO: fix me
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{iu9lab}[2024/03/06 LaTeX class for IU9 lab reports]

\LoadClass[a4paper, 12pt, oneside, titlepage]{extarticle}

% Page geometry
\RequirePackage[bottom=3cm]{geometry}

% Text indentation
\RequirePackage{indentfirst}

% Better page header/footer
\RequirePackage{fancyhdr}
\fancyhf{}
\renewcommand{\headrulewidth}{0pt}
\cfoot{\thepage}
\pagestyle{fancy}

% Russian-specific packages
\RequirePackage[T2A]{fontenc}
\RequirePackage[utf8]{inputenc} 
\RequirePackage[english, main=russian]{babel}

% Code listings
\RequirePackage[newfloat]{minted}
\RequirePackage{caption, subcaption}
\captionsetup[subfigure]{skip=0.5ex}
\setminted[java]{breaklines, frame=single, framesep=10pt}
\setminted[cpp]{breaklines, frame=single, framesep=10pt}
\newenvironment{code}{\captionsetup{type=listing}}{}
\SetupFloatingEnvironment{listing}{name=Код}

% Better section title
\RequirePackage[md]{titlesec}
\titlelabel{\thetitle.\quad}
\titleformat*{\section}{\LARGE\mdseries}
\titleformat*{\subsection}{\Large\mdseries}

% Hyperlinks
\usepackage{hyperref}
\usepackage[usenames,dvipsnames]{xcolor}

\hypersetup{
    colorlinks,
    linkcolor={black},
    citecolor={black},
    urlcolor={blue!80!black},
	unicode=true
}

% Images
\RequirePackage{graphicx}
\graphicspath{{./images/}}

% Title page configuration
\def\theauthor{}
\newcommand{\course}[1]{\def\thecourse{#1}}
\renewcommand{\title}[1]{\def\thetitle{#1}}
\newcommand{\teacher}[1]{\def\theteacher{#1}}
\renewcommand{\author}[1]{\def\theauthor{#1}}
\newcommand{\group}[1]{\def\thegroup{#1}}
\newcommand{\labnumber}[1]{\newcommand{\thenumber}{%
    \def\temp{#1}%
    \ifx\temp\empty
        %
    \else
        №\temp
    \fi%
}}

\renewcommand{\maketitle}
{
\begingroup
\fontsize{14}{16}\selectfont
\newgeometry{
  left=2cm,
  right=2cm,
  top=2cm,
  bottom=2cm
}
\begin{titlepage}
    \centering

    Федеральное государственное бюджетное образовательное \\
    учреждение высшего образования \\
    <<Московский государственный технический университет\\
    имени Н.Э. Баумана>>\\
    (МГТУ им. Н.Э. Баумана)

    \vspace{0.7cm}

    \flushleft

    Факультет: \underline{Информатика и системы управления}\\
    Кафедра: \underline{Теоретическая информатика и компьютерные технологии}

    \centering
    \topskip0pt

    \vspace*{\fill}
    \begingroup
        \Large
        Лабораторная работа \thenumber\\
        \large
        по курсу: <<\thecourse>>\\
        <<\thetitle>>\\
    \endgroup
    \vspace*{\fill}

    \hfill
    \begin{minipage}{0.4\linewidth}
        Выполнил:\\
        Студент группы \thegroup\\
        \theauthor\\[1.5mm]
        Проверил:\\
        \theteacher
    \end{minipage}
    \vfill

    Москва, \the\year

\end{titlepage}
\restoregeometry{}
\setcounter{page}{2}
\endgroup
}
