\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{AIMSreport}[2014/08/16 Example LaTeX class]

\LoadClass[a4paper,10pt]{extarticle}

\usepackage{fancyhdr}
\usepackage{xcolor}
\usepackage{color,colortbl}
\usepackage{etoolbox}
\usepackage{graphicx}
\usepackage[space]{grffile}
\usepackage{fontspec}
\usepackage[explicit]{titlesec}
\usepackage{eso-pic}
\usepackage{tabularx}
\usepackage{longtable}
\usepackage{ltxtable}
\usepackage{multirow}
\usepackage{rotating}
\usepackage{booktabs}
\usepackage[lmargin=2.54cm,rmargin=2.54cm, tmargin=2.54cm,bmargin=2.54cm,headsep=1.52cm,a4paper]{geometry}

%% Some choices of math font
%\usepackage{sfmath} %redefines math font into sf
%\usepackage{MnSymbol}
%\usepackage{lmodern}
%\usepackage{mathpazo}
%\usepackage{kpfonts}
%\usepackage{mathptmx}
%\usepackage{times,mtpro2}
%\usepackage{stix} % cant do large curly braces
\usepackage{txfonts}
%\usepackage{newtxtext,newtxmath}
%\usepackage{libertine} 
%\usepackage[libertine]{newtxmath}
%\usepackage{newpxtext}
%\usepackage[euler-digits]{eulervm}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\usepackage{mathtools} %recently added to fix an issue with pmatrix, matrix, bmatrix, align etc
\usepackage{enumitem}% http://ctan.org/pkg/enumitem
\usepackage{tikz}
\usetikzlibrary{backgrounds}
\usetikzlibrary{shadows}
\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt plus 2pt minus 1pt}
\setlength{\emergencystretch}{3em}  % prevent overfull lines
\usepackage{lscape}
\usepackage{setspace}
\usepackage{lastpage}
\usepackage{graphbox}

%%-----Define Provision for bibliography-----------
\usepackage{natbib}
\usepackage{bibentry}
%%-------------------------------------------------


\newcommand\VRule[1][\arrayrulewidth]{\vrule width #1}
\newcommand\whiteHeader[1]{\color{white}{\textbf{\textsc{\normalsize #1}}}}
\definecolor{shade}{HTML}{D4D7FE}		%light blue shade

%----------------------------------------------------------------------------------------
\usepackage{etoolbox} % to allow control of the linespacing in the table of contents
\makeatletter
\pretocmd{\section}{\addtocontents{toc}{\protect\addvspace{5\ex}}}{}{}
\pretocmd{\subsection}{\addtocontents{toc}{\protect\addvspace{-5\ex}}}{}{}
\pretocmd{\subsubsection}{\addtocontents{toc}{\protect\addvspace{-5\ex}}}{}{}
\makeatother
% ----------------------------------------------------------------------------------------

%------------------------------------------------------------------------------------------
%fix an issue with xelatex and grffile for spaces and dots in includegraphics filenames
\makeatletter
\def\Gread@@xetex#1{%
  \IfFileExists{"\Gin@base".bb}%
  {\Gread@eps{\Gin@base.bb}}%
  {\Gread@@xetex@aux#1}%
}
\makeatother
% -----------------------------------------------------------------------------------------


\setmainfont[BoldFont={GillSans-Bold}, ItalicFont={GillSans-Italic}]{GillSans}
\newfontfamily\titlefont[Color=black]{GillSans-Bold}
\newfontfamily\subtitlefont[Color=black]{GillSans-Bold} %{Arial}
\newfontfamily\sectionfont[Color=blue]{GillSans-Bold} %{Arial}
\newfontfamily\subsectionfont[Color=black]{GillSans-Bold}
\newfontfamily\paragraphfont[Color=black]{GillSans-Italic}

\definecolor{AIMSblue}{RGB}{0,51,102}

%----------------------------------------------------------------------------------------
%     Define the header and footers
%----------------------------------------------------------------------------------------
\newcommand{\headrulecolor}[1]{\patchcmd{\footrule}{\hrule}{\color{#1}\hrule}{}{}}
\fancypagestyle{firststyle}
{
   \renewcommand{\headrulewidth}{0pt}%
   \fancyhf{}
   \fancyhead[C]{}%
   \fancyfoot[C]{}%
   \renewcommand{\headrulewidth}{0pt}%
   \renewcommand{\footrulewidth}{0pt}%
}
\pagestyle{fancy}
\fancyhf{} % clear all header and footer fields
\renewcommand{\headrulewidth}{1.5pt}%
\renewcommand{\headrule}{\hbox to\headwidth{%
    \color{blue!70}\leaders\hrule height \headrulewidth\hfill}}
% \renewcommand{\subsectionmark}[1]{\markboth{#1}{}}
\let\Sectionmark\sectionmark
\def\sectionmark#1{\def\Sectionname{#1}\Sectionmark{#1}}
\let\Subsectionmark\subsectionmark
\def\subsectionmark#1{\def\Subsectionname{#1}\Subsectionmark{#1}}
\fancyhead[C]{}
%\fancyhead[L]{\raisebox{-.5\baselineskip}[0pt][0pt]{\parbox{0.4\textwidth}{\leftmark}}}
%\fancyhead[R]{\raisebox{-.5\baselineskip}[0pt][0pt]{\parbox{0.4\textwidth}{\hfill\rightmark}}}
\fancyhead[L]{\raisebox{-.5\baselineskip}[0pt][0pt]{\parbox{0.4\textwidth}{\rightmark}}}
\fancyhead[R]{\raisebox{-.5\baselineskip}[0pt][0pt]{\parbox{0.4\textwidth}{\hfill\thesubsection\quad \Subsectionname}}}
\fancyfoot[C]{}
\fancyfoot[L]{\parbox{0.8\textwidth}{Water Quality Report Card Metrics}}
\fancyfoot[R]{Page~\thepage}
\renewcommand{\footrulewidth}{1.5pt}%
\headrulecolor{blue!70}%

% \usepackage{titleps}
% \newpagestype{mypage}{%
%   \headrule
%   \sethead{\MakeUppercase{\thesection\quad\sectiontitle}}{}{\thesubsection\quad\subsectiontitle}
%   \setfoot{Water Quality Report Card Matrics}{}{\thepage}
% }
% \settitlemarks{section,subsection}
% \pagestyle{mypage}




%% %-Redefine the section title formats--------------
\setcounter{secnumdepth}{4}
\titleformat{\chapter}% which section
            {\fontsize{20}{15}\bfseries\chapterfont\raggedright\color{blue}}% section label
            {\fontsize{20}{15}\raggedright\bfseries\chapterfont\color{blue}\thechapter~}% section number
            {1ex}% separator space
            {\MakeUppercase{#1}} %before code
\titleformat{\section}% which section
            {\Large\bfseries\sectionfont\raggedright\color{blue}}% section label
            {\Large\raggedright\bfseries\sectionfont\color{blue}\thesection.~}% section number
            {1ex}% separator space
            {\MakeUppercase{#1}} %before code          
\titlespacing{\section}{0pt}%left
             {1.5ex}%before
             {1.0ex}%after             
\titleformat{\subsection}% which section
            {\large\raggedright\subsectionfont\color{black}}% section label
            {\large\raggedright\subsectionfont\color{black}\thesubsection~}% section number
            {1ex}% separator space
            {#1} %before code
\titlespacing{\subsection}{0pt}%left
             {1.5ex}%before
             {1.0ex}%after             
\titleformat{\subsubsection}% which section
            {\normalsize\raggedright\subsectionfont\color{black}}% section label
            {\normalsize\raggedright\subsectionfont\color{black}\thesubsubsection~}% section number
            {1ex}% separator space
            {#1} %before code
\titlespacing{\subsubsection}{0pt}%left
             {1.1ex}%before
             {1.0ex}%after             
\titleformat{\paragraph}% which section
            {\normalsize\raggedright\paragraphfont\itseries\color{black}}% section label
            {\normalsize\raggedright\paragraphfont\color{black}\theparagraph~}% section number
            {1ex}% separator space
            {#1} %before code
\titlespacing{\paragraph}{0pt}%left
             {1.1ex}%before
             {1.0ex}%after
\setlength{\parindent}{0pt}
