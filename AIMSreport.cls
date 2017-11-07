\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{exampleclass}[2014/08/16 Example LaTeX class]

\LoadClass[11pt]{article}

\usepackage{fancyhdr}
\usepackage{xcolor}
\usepackage{color,colortbl}
\usepackage{etoolbox}
\usepackage{grffile}
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
\usepackage{sfmath} %redefines math font into sf
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

%%-----Define Provision for bibliography-----------
\usepackage{natbib}
\usepackage{bibentry}
%%-------------------------------------------------


\newcommand\VRule[1][\arrayrulewidth]{\vrule width #1}
\newcommand\whiteHeader[1]{\color{white}{\textbf{\textsc{\normalsize #1}}}}
\definecolor{shade}{HTML}{D4D7FE}		%light blue shade

%----------------------------------------------------------------------------------------
%   Define fonts
%----------------------------------------------------------------------------------------
%\setmainfont[ExternalLocation=~/.fonts/, BoldFont={GillSans-Bold}, ItalicFont={GillSans-Italic}]{GillSans}
%\setmainfont[ExternalLocation=~/.fonts/, BoldFont={GillSans-Bold}, ItalicFont={GillSans-Italic}]{GillSans}
%\setmainfont[Path=/home/l-p/mlogan/.fonts/,Extension= .tff, BoldFont={gillsans-bold}, ItalicFont={gillsans-italic}]{gillsans}
%\setmainfont{GillSans}[
%  Path=/home/l-p/mlogan/.fonts/,
%  Extension= .ttf]
%\setmainfont{GillSans}[
%  Path=../../../../.fonts/,
%  Extension= .ttf]
%\fontspec[ExternalLocation=/home/l-p/mlogan/.fonts/, Extension= .ttf]{gillsans}
%\fontspec{GillSans}
\setmainfont{GillSans}
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
\renewcommand{\headrulewidth}{0pt}%
\fancyfoot[C]{}
\fancyfoot[L]{\parbox{0.8\textwidth}{Murray Logan}}
\fancyfoot[R]{Page~\thepage}
\renewcommand{\footrulewidth}{1.5pt}%
\headrulecolor{blue!70}%


%% %-Redefine the section title formats--------------
\setcounter{secnumdepth}{4}
\titleformat{\chapter}% which section
            {\fontsize{20}{15}\bfseries\chapterfont\raggedright\color{blue}}% section label
            {\fontsize{20}{15}\raggedright\bfseries\chapterfont\color{blue}\thechapter~}% section number
            {1ex}% separator space
            {\MakeUppercase{#1}} %before code
\titleformat{\section}% which section
            {\fontsize{20}{15}\bfseries\sectionfont\raggedright\color{blue}}% section label
            {\fontsize{20}{15}\raggedright\bfseries\sectionfont\color{blue}\thesection~}% section number
            {1ex}% separator space
            {\MakeUppercase{#1}} %before code
\titlespacing{\section}{0pt}%left
             {30pt}%before
             {30pt}%after

\titleformat{\subsection}% which section
            {\fontsize{14}{14}\raggedright\subsectionfont\color{black}}% section label
            {\fontsize{14}{14}\raggedright\subsectionfont\color{black}\thesubsection~}% section number
            {1ex}% separator space
            {#1} %before code
\titlespacing{\subsection}{0pt}%left
             {18pt}%before
             {12pt}%after

\titleformat{\subsubsection}% which section
            {\fontsize{11}{11}\raggedright\subsectionfont\color{black}}% section label
            {\fontsize{11}{11}\raggedright\subsectionfont\color{black}\thesubsubsection~}% section number
            {1ex}% separator space
            {#1} %before code
\titlespacing{\subsubsection}{0pt}%left
             {10pt}%before
             {6pt}%after

\titleformat{\paragraph}% which section
            {\fontsize{11}{11}\raggedright\paragraphfont\itseries\color{black}}% section label
            {\fontsize{11}{11}\raggedright\paragraphfont\color{black}\theparagraph~}% section number
            {1ex}% separator space
            {#1} %before code
\titlespacing{\paragraph}{0pt}%left
             {10pt}%before
             {6pt}%after

%\titleformat{\appendix}
%            {\fontsize{20}{15}\bfseries\sectionfont\raggedright\color{blue}}% section label
%            {\fontsize{20}{15}\raggedright\bfseries\sectionfont\color{blue}\thesection~}% section number
%            {1ex}% separator space
%            {\MakeUppercase{#1}} %before code
\setlength{\parindent}{0pt}

\usepackage{apptools, etoolbox}
\newcommand\appendix{\par   % line 581
  \setcounter{section}{0}% 
  \setcounter{subsection}{0}%
  \gdef\@sectapp{\appendixname}%
  \gdef\thesection{\@Alph\c@section}}

\newcommand\appendixname{APPENDIX} % line 784

\makeatletter
\patchcmd{\@section}{\protect {SECTION }}{\ifappendix{APPENDIX }\else{SECTION }\fi}{}{}
\makeatother


             