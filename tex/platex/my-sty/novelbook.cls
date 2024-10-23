\NeedsTeXFormat{pLaTeX2e}

%
% novelbook.cls
%
% pLaTeX2e class file for Novels
%

\ProvidesClass{novelbook}
\LoadClass[a5paper,twoside,notitlepage,openany,twocolumn]{tbook}

%% パッケージ
\usepackage[dvipdfmx]{graphicx,color}	% PDF用
%\usepackage[dviout]{graphicx}		% dvi用
\usepackage{multicol}
\usepackage{color}
\usepackage{tatesuji}
\usepackage{furikana}
\usepackage{wallpaper}

%% マージン
% A5縦 210mm = ヘッダ 6mm + 本文 198mm + フッタ 6mm
% A5横 148mm = 小口 24mm + 本文 100mm + のど 24mm

% 本文
\textheight=100mm	% 横幅
\textwidth=198mm	% 高さ

% ヘッダ
\topmargin=-1in \advance\topmargin 2mm
\headheight=2mm
\headsep=2mm

% フッタ
\footskip=4mm

% 小口
\oddsidemargin =-1in \advance\oddsidemargin 24mm
\evensidemargin=-1in \advance\evensidemargin 24mm

%% 段落間スペース
\columnsep=6mm

%% 段落先頭のインデント
\parindent=0pt

%% ページスタイル：小口に漢数字のページ番号と背景画像(lineR.bmp, lineL.bmp)
\renewcommand{\ThisLLCornerWallPaper}[2]{%
  \AddToShipoutPicture*{%
    \AtPageLowerLeft{%
      \parbox[b][\paperwidth][b]{\paperheight}{%       
        \vfill%
        \includegraphics[height=#1]{#2}%
        \vfill%
      }
    }
  }
}

\renewcommand{\ThisULCornerWallPaper}[2]{%
  \AddToShipoutPicture*{%
    \AtPageLowerLeft{%
      \parbox[b][\paperheight][t]{\paperwidth}{%       
        \includegraphics[height=#1]{#2}%
      }
    }
  }
}

\def\ps@nvlBaseStyle{\let\@mkboth\@gobbletwo
   \let\ps@jpl@in\ps@plain
   \def\@oddfoot{%
      \ThisLLCornerWallPaper{21.55mm}{img/lineL.bmp}
      \llap{%
         \smash{%
            \dimen@.5\textheight \advance\dimen@\footskip
            \vbox to\dimen@{%
               \hbox{\tate \rightmark}%
               \vfil
               \hbox{\tate\Tatesuji[0]{page}　　}}}%
         \hskip10mm}%
      \hfil}%
   \def\@evenfoot{%
      \ThisULCornerWallPaper{21.55mm}{img/lineR.bmp}
      \hfil
      \rlap{%
         \hskip10mm
         \smash{%
            \dimen@.5\textheight \advance\dimen@\footskip
            \vbox to\dimen@{%
               \hbox{\tate \leftmark}%
               \vfil
               \hbox{\tate\Tatesuji[0]{page}　　}}}}}%
   \let\@oddhead\@empty
   \let\@evenhead\@empty
   \def\chaptermark##1{\markboth{\thechapter \quad ##1}{}}%
   \def\sectionmark##1{\markright{\thesection\quad ##1}}}

%% ページスタイル：ページ番号、画像なし
\def\ps@nvlNoPageNo{\let\@mkboth\@gobbletwo
   \let\ps@jpl@in\ps@plain
   \def\@oddfoot{%
      \llap{%
         \smash{%
            \dimen@.5\textheight \advance\dimen@\footskip
            \vbox to\dimen@{%
               \hbox{\tate \rightmark}%
               \vfil
               \hbox{}}}%
         \hskip5mm}%
      \hfil}%
   \def\@evenfoot{%
      \hfil
      \rlap{%
         \hskip5mm
         \smash{%
            \dimen@.5\textheight \advance\dimen@\footskip
            \vbox to\dimen@{%
               \hbox{\tate \leftmark}%
               \vfil
               \hbox{}}}}}%
   \let\@oddhead\@empty
   \let\@evenhead\@empty
   \def\chaptermark##1{\markboth{\thechapter \quad ##1}{}}%
   \def\sectionmark##1{\markright{\thesection\quad ##1}}}

%% 部タイトル
\renewcommand{\part}{%
  \clearpage
  \ifodd\c@page\else\hbox{}\newpage\fi
  \thispagestyle{nvlNoPageNo}%
  \if@twocolumn\onecolumn\@tempswatrue\else\@tempswafalse\fi
  \null\vfil
  \secdef\@part\@spart}
\def\@part[#1]#2{%
  \ifnum \c@secnumdepth >-2\relax
    \refstepcounter{part}%
    \addcontentsline{toc}{part}{%
       \prepartname\thepart\postpartname\hspace{1em}#1}%
  \else
    \addcontentsline{toc}{part}{#1}%
  \fi
  \markboth{}{}%
  {\centering
   \interlinepenalty\@M\reset@font
   \ifnum \c@secnumdepth >-2\relax
     \huge\bfseries\prepartname\thepart\postpartname
     \par\vskip20\p@
   \fi
   \Huge\bfseries#2\par}%
   \@endpart}
\def\@spart#1{{%
  \centering
  \interlinepenalty\@M\reset@font
  \Huge#1\par}%
  \@endpart}
\def\@endpart{%
   \if@twoside\null\thispagestyle{empty}\newpage\fi
   \if@tempswa\twocolumn\fi}

%% 章タイトル
\renewcommand{\chapter}{%
  \thispagestyle{nvlBaseStyle}%
  \newpage
  \global\@topnum\z@
  \@afterindenttrue
  \secdef\@chapter\@schapter}
\def\@chapter[#1]#2{%
  \ifnum \c@secnumdepth >\m@ne
    \if@mainmatter
    \refstepcounter{chapter}%
    \typeout{\@chapapp\space\thechapter\space\@chappos}%
    \addcontentsline{toc}{chapter}%
      {\protect\numberline{\@chapapp\thechapter\@chappos}#1}%
    \else\addcontentsline{toc}{chapter}{#1}\fi
  \else
    \addcontentsline{toc}{chapter}{#1}%
  \fi
  \chaptermark{#1}%
  \addtocontents{lof}{\protect\addvspace{10\p@}}%
  \addtocontents{lot}{\protect\addvspace{10\p@}}%
  \@makechapterhead{#2}\@afterheading}
\def\@makechapterhead#1{\hbox{}%
  \vskip2\Cvs
  {\parindent\z@
   \raggedright
   \reset@font\huge\bfseries
   \leavevmode
   \ifnum \c@secnumdepth >\m@ne
     \setlength\@tempdima{\linewidth}%
    \if@mainmatter
     \setbox\z@\hbox{\@chapapp\thechapter\@chappos\hskip1zw}%
     \addtolength\@tempdima{-\wd\z@}%
     \unhbox\z@\nobreak
    \fi
     \vtop{\hsize\@tempdima#1}%
   \else
     #1\relax
   \fi}\nobreak\vskip3\Cvs}
\def\@schapter#1{%
  \@makeschapterhead{#1}\@afterheading
}
\def\@makeschapterhead#1{\hbox{}%
  \vskip1\Cvs
  {\parindent\z@
   \raggedright
   \reset@font\huge
   \leavevmode
   \setlength\@tempdima{\linewidth}%
   \vtop{\hsize\@tempdima #1}}\vskip2.5\Cvs}

%% 目次
\def\addcontentsline#1#2#3{%
  \protected@write\@auxout
    {\let\label\@gobble \let\index\@gobble \let\glossary\@gobble
\@temptokena{\thepage}}%
    {\string\@writefile{#1}%
       {\protect\contentsline{#2}{#3}{\the\@temptokena}}}%
}
\renewcommand{\tableofcontents}{%
  \@starttoc{toc}%
  \if@restonecol\twocolumn\fi
}
\renewcommand*{\l@part}[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty{-\@highpenalty}%
    \addvspace{2.25em \@plus\p@}%
    \begingroup
    \parindent\z@\rightskip\@pnumwidth
    \parfillskip-\@pnumwidth
    {\leavevmode\large
     \setlength\@lnumwidth{4zw}%
     #1\hfil\nobreak
     \hbox to\@pnumwidth{\hss \tatesuji{#2}}}\par
    \nobreak
    \global\@nobreaktrue
    \everypar{\global\@nobreakfalse\everypar{}}%
     \endgroup
  \fi}
\def\threedotsleader{%
  \leaders\hbox to.3333zw{\hss\m@th$\cdot$\hss}\hfill}%
\renewcommand*{\l@chapter}[2]{%
  \ifnum \c@tocdepth >\m@ne
    \addpenalty{-\@highpenalty}%
    \addvspace{1.0em \@plus\p@}%
    \begingroup
      \parindent\z@ \rightskip\@pnumwidth \parfillskip-\rightskip
      \leavevmode
      \setlength\@lnumwidth{4zw}%
      \advance\leftskip\@lnumwidth \hskip-\leftskip
      #1\nobreak\threedotsleader\nobreak\hbox{\hss \tatesuji{#2}}\par
      \penalty\@highpenalty
    \endgroup
  \fi}

%% 文字サイズと行間
\def\normalsize{%
  \@setfontsize\normalsize{9pt}{15pt}}

%% 天地中央表示
\newenvironment{vhcenter}{%
  \thispagestyle{nvlNoPageNo}%
  \if@twocolumn\onecolumn\@tempswatrue\else\@tempswafalse\fi
  \null\vfil
  \interlinepenalty\@M\reset@font
  \begin{center}}{%
  \end{center}
  \if@twoside\null\thispagestyle{empty}\newpage\fi
  \if@tempswa\twocolumn\fi}

%% ページスタイル
\pagestyle{nvlBaseStyle}

%% 段落抜きタイトル
\newcommand{\titleOneColumn}[2]{%
  \twocolumn[\vspace{#2} \Huge #1 \vspace{#2}]}

%% ダッシュ
\def\dash{\rule[-0.25Q]{2zw}{0.5Q}}

%% ！？の後の空白
\def\spaceaftqae{\kern -0.1zw}
