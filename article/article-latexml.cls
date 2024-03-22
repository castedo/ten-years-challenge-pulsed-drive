%% Significantly altered copy of:
%% ReScience article template
%% Copyright (c) 2018 N.P. Rougier,
%% Released under a Creative Commons Attribution 4.0 International license.

\NeedsTeXFormat{LaTeX2e}

\LoadClass[a4paper, oneside, 10pt]{article}

% --- Langage -----------------------------------------------------------------
\RequirePackage[american]{babel}


% --- Various packages --------------------------------------------------------
\RequirePackage{ifthen}
\RequirePackage{tabularx}
\RequirePackage{tikz}
\RequirePackage{marginnote}


% --- Mathematics -------------------------------------------------------------
\RequirePackage{amsmath}
\RequirePackage{mathtools}

% --- Page layout -------------------------------------------------------------
\RequirePackage[a4paper]{geometry}
\geometry{left=4.5cm, right=3cm, top=3cm, bottom=3.5cm,
          heightrounded, headheight=12.1pt,
          marginparwidth=3cm, marginparsep=0.75cm, footskip=2cm}
\reversemarginpar


% --- Font stack --------------------------------------------------------------

%% PDFLaTeX
%% \RequirePackage[light,condensed]{roboto} % Sans Serif
%% \RequirePackage[default,light,semibold,tabular]{sourceserifpro} % Roman
%% \RequirePackage[varqu]{zi4} % Monotype
%% \makeatletter
%% \def\bfseries@sf{sb}
%% \makeatother

% XeLaTeX
\RequirePackage{fontspec}
\defaultfontfeatures{Ligatures=TeX}
\setmainfont{Source Serif Pro}[
  Path = ./source-serif-pro/SourceSerifPro-, Extension = .otf,
  UprightFont= Light,
  ItalicFont = LightIt,
  BoldFont   = Regular,
  BoldItalicFont = It,
]
\setsansfont{Roboto Condensed}[
  Path = ./roboto/RobotoCondensed-, Extension = .ttf,
  UprightFont = Light,
  BoldFont = Regular
]

\setmonofont{Source Code Pro}[
  Path = ./source-code-pro/SourceCodePro-, Extension = .otf,
  UprightFont = Light,
  BoldFont = Regular
]

\newfontfamily\RobotoCon{Roboto Condensed}[
  Path = ./roboto/RobotoCondensed-, Extension = .ttf,
  UprightFont = Regular,
  BoldFont = Bold
]  

\newfontfamily\Roboto{Roboto}[
  Path = ./roboto/Roboto-, Extension = .ttf,
  UprightFont = Regular,
  BoldFont = Black
]  

\newfontfamily\SpaceRoboto{Roboto}[
  Path = ./roboto/Roboto-, Extension = .ttf,
  UprightFont = Regular,
  BoldFont = Medium,
  UprightFeatures={LetterSpace=35},
  BoldFeatures={LetterSpace=35}
]  


\RequirePackage{microtype}


% --- Captions ----------------------------------------------------------------
\RequirePackage[labelsep=period]{caption}
\renewcommand{\captionfont}{\small}
\renewcommand{\captionlabelfont}{\small\sffamily\bfseries}
%% \captionsetup[figure]{justification=justified, singlelinecheck=false}
%% \captionsetup[table]{justification=justified, singlelinecheck=false}
%% \captionsetup[listing]{justification=justified, singlelinecheck=false}


% --- Hyperref & colors -------------------------------------------------------
\RequirePackage[pdfusetitle,
            bookmarks=true,
            breaklinks=true,
            pdfborder={0 0 0},
            citecolor=citecolor,
            linkcolor=linkcolor,
            urlcolor=urlcolor,
            colorlinks=true,
            linktocpage=false,
            hyperindex=true,
            colorlinks=true,
            linktocpage=false,
            linkbordercolor=white]{hyperref}
\hypersetup{linkcolor=black,urlcolor=darkgray}
\urlstyle{sf}
