\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{response}[2021]
% article inheritance
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}
% packages
\RequirePackage[margin=3cm]{geometry}
\RequirePackage{authblk}
\RequirePackage[most]{tcolorbox}
\RequirePackage{titlesec}
% spacing
\def\baselinestretch{1.7}
\setlength{\parskip}{1ex}
\setlength{\parindent}{0ex}
% meta
\renewcommand{\title}[1]{\gdef\@title{#1}}
\newcommand{\refid}[1]{\gdef\@refid{#1}}
% helpers
\renewcommand{\paragraph}[1]{\par\textbf{#1:}}
\newcommand{\tpart}[3][:]{%
  \hspace{0.14\linewidth}\llap{\textbf{#2#1}}\hspace{0.01\linewidth}
  \noindent\parbox[t]{0.85\textwidth}{#3}\vskip 1em}
% maketitle
\renewcommand{\maketitle}{%
  \thispagestyle{plain}
  \tpart[]{}{\textbf{\Large{Response to Reviewers' Comments}}}
  \tpart{Re}{\@refid}
  \tpart{Title}{\@title}
  \tpart{Authors}{\@author}
  \tpart{Date}{\@date}
  \clearpage}
% review elements
\titleformat{\subsubsection}[runin]{\normalsize\bfseries}{\llap{\thesubsubsection\hspace{1em}}}{0pt}{}
\titlespacing{\subsubsection}{0pt}{3ex plus 1ex minus .2ex}{0pt}
\renewenvironment{comment}[1][]{\subsubsection#1{}\ignorespaces}{\par}
\newenvironment{response}{\begin{rbox}}{\end{rbox}}
\newtcolorbox{rbox}[1][]{
  enhanced, frame hidden, sharp corners, boxrule=0pt, breakable, parbox=false,
  borderline west = {1pt}{0pt}{solid}}
\renewcommand{\quote}[1]{``\textit{#1}''}