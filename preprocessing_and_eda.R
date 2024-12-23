# Authors: Bazzan Sofia, Pasin Diletta
# Description: Preprocessing, Exploratory Data Analysis, and Train/Test Split for Spam Detection Project

# First, here there is a brief explanation of each variable
\begin{itemize}
\item  \textbf{Word frequency}: There are 48 continuous real attributes between 0 and 100  of the type word\_freq\_WORD that represents the percentage of words in the e-mail that match WORD, i.e. 100 * (number of times the WORD appears in the e-mail) / total number of words in e-mail.

(The variables of this type are : word\_freq\_make, word\_freq\_address, 
word\_freq\_all, word\_freq\_3d, word\_freq\_our, word\_freq\_over,
 word\_freq\_remove, word\_freq\_internet, word\_freq\_order, word\_freq\_mail, word\_freq\_receive, word\_freq\_will, word\_freq\_people, word\_freq\_report, word\_freq\_addresses, word\_freq\_free, word\_freq\_business, word\_freq\_email, word\_freq\_you, word\_freq\_credit, word\_freq\_your, word\_freq\_font,
  word\_freq\_000, word\_freq\_money, word\_freq\_hp, word\_freq\_hpl, word\_freq\_george, word\_freq\_650, word\_freq\_lab, word\_freq\_labs, word\_freq\_telnet, word\_freq\_857, word\_freq\_data, word\_freq\_415, 
  word\_freq\_85, \sloppy
  word\_freq\_technology, word\_freq\_1999, word\_freq\_parts, 
  word\_freq\_pm, word\_freq\_direct, word\_freq\_cs, word\_freq\_meeting, word\_freq\_original,
  word\_freq\_project, word\_freq\_re, word\_freq\_edu, word\_freq\_table, word\_freq\_conference)
\item  \textbf{Char frequency}: 6 of the attributes are frequency of characters in the mail. In particular their type could be defined as char\_freq\_CHAR that describe the percentage of characters in the e-mail that match CHAR, i.e. 100 * (number of CHAR occurences) / total characters in e-mail. Like the word frequencies also this variables are continuous real attributes between 0 and 100.

(To be more precise, this type of variables are the following: char\_freq\_.3B (;), char\_freq\_.28 ((), char\_freq\_.5B ([), char\_freq\_.21 (!), char\_freq\_.24 (\$), char\_freq\_.23 (\# )

\item  \textbf{capital\_run\_length\_average} 1 continuous real attribute of type capital\_run\_length\_average which denotes the average length of uninterrupted sequences of capital letters
\item  \textbf{capital\_run\_length\_longest} 1 continuous real attribute of type capital\_run\_length\_longest that is equal to the length of longest uninterrupted sequence of capital letters
\item  \textbf{capital\_run\_length\_total}  1 continuous real attribute that indicate the total number of capital letters in the e-mail
\item  \textbf{class} 1 nominal {0,1} class attribute of type spam that  denotes whether the e-mail was considered spam (1) or not (0)
\end{itemize}
                                                                                                                                                    
#Preprocessing                                                                                                                                                                
