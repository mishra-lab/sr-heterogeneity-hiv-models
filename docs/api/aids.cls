\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{aids}[2021]
% article inheritance
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}
% packages
\RequirePackage[margin=3cm]{geometry}
\RequirePackage{authblk}
% spacing
\def\baselinestretch{1.7}
\setlength{\parskip}{1ex}
\setlength{\parindent}{0ex}
\setlength{\abovecaptionskip}{1\baselineskip}
\setlength{\belowcaptionskip}{1\baselineskip}
\widowpenalty10000
\clubpenalty10000
% meta-data
\def\dummy{NYLL}
\newcommand{\shorttitle}[1]{\gdef\@shorttitle{#1}}
\newcommand{\funding}[1]{\gdef\@funding{#1}}
\newcommand{\coi}[1]{\gdef\@coi{#1}}
\newcommand{\acknowl}[1]{\gdef\@acknowl{#1}}
\newcommand{\contrib}[1]{\gdef\@contrib{#1}}
\newcommand{\words}[2]{\gdef\@abswords{#1}\gdef\@words{#2}}
\newcommand{\keywords}[1]{\gdef\@keywords{#1}}
% helpers
\renewcommand{\paragraph}[1]{\par\textbf{#1:}}
\newcommand{\headstyle}[1]{\normalfont\textsc{\MakeLowercase{#1}}}
\newcommand{\tpart}[2]{%
  \hspace{0.14\linewidth}\llap{\textbf{#1:}}\hspace{0.01\linewidth}
  \noindent\parbox[t]{0.85\textwidth}{#2}\vskip 1em}
% maketitle
\renewcommand{\maketitle}{%
  \thispagestyle{plain}
  \tpart{Title}{\@title}
  \tpart{Short Title}{\@shorttitle}
  \tpart{Authors}{\@author}
  \tpart{Date}{\@date}
  \tpart{Funding}{\@funding}
  \tpart{Conflicts of Interest}{\@coi}
  \tpart{Words}{\@words~/~3500}
  \clearpage}
% abstract
\let\xabstract\abstract
\def\abstract{%
  \thispagestyle{plain}
  \centering\headstyle{\@title}\par
  \vskip1em
  \xabstract\par\noindent}
\let\xendabstract\endabstract
\def\endabstract{%
  \vskip1em\noindent\textbf{Words:}~\@abswords~/~250
  \vskip1em\noindent\textbf{Keywords:}~\@keywords
  \xendabstract\par\clearpage}
% running head
\renewcommand\rightmark{\headstyle{\@shorttitle}}
\pagestyle{headings}
% back matter
\def\backmatter{%
  \clearpage
  \subsection*{Conflicts of Interest}\@coi
  \subsection*{Funding}\@funding
  \subsection*{Acknowledgements}\@acknowl
  \subsection*{Contributions}\@contrib
  \clearpage
}
% appendix
\let\xappendix\appendix
\renewcommand{\appendix}{
  \xappendix
  \clearpage
  \counterwithin{table}{section}
  \counterwithin{figure}{section}
  {\large\textsc{appendix}}
  \vskip1em
  \tpart{Title}{\@title}
  \tpart{Authors}{\@author}
  \def\baselinestretch{1.1}
  \refsection
  \clearpage
}