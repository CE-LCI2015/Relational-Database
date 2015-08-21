# **Costa Rica Institute of Technology**
**Programming Languages, Compilers and Interpreters**

**Relational Database**

**Project by: Roberto Bonilla and Pablo Rodríguez**

# Introduction

Nowadays databases are very important on the Internet. Most companies use Relational Databases. We are developing on LISP/SCHEME
a simple relational database. As part of this project we are going to learn how to program in a functional language interacting with their syntax and environment.

# User Guide

This project is developed in Dr. Racket, you need to download the interpreter and ide in order to run the program. You only have to load the file
 and click on run . After that the prompt will be displayed on the screen and you have to choose a command from the following list:
 
 * addtable / addt
 * insert / ins
 * addReference / addr
 * removeReference / remr
 * update / ud
 * remover / rr
 * deltable / delt
 * query
 * cproc
 * eval
 
 
## Test Cases
 ![Screenshot of UI](http://s11.postimg.org/iae61su8z/Screen_Shot_2015_08_21_at_3_45_09_AM.png)
 
 
 **Example of addtable, insert**

 ![Screenshot of UI](http://s30.postimg.org/gzuutbnb5/Screen_Shot_2015_08_21_at_3_49_58_AM.png)
 
 
 **Test case of update, showall**
 
 
 ![Screenshot of UI](http://s9.postimg.org/z1wlpjq0v/Screen_Shot_2015_08_21_at_4_56_34_AM.png)
 
 
 **Example how to use cproc and eval**
  
  
  
# Development environment

In this project it was used the IDE of Dr Racket were it was used to write and debug code. Also it was managed by GitHub to organize and share 
the code with my partner. The CVS integration of Clion was the key of successful teamwork.

# Data Structures and functions

The database was implemented using lists as tables and inside the table we have a table in which we include a header with the name, number of 
foreign keys and the columns. As follows:
 
  {(tableName 0 id rows...)(1 data...)(2 data...)} 
  
Also we have a header for the database where we store the procedures to be executed. 
 
 * AddTable:
 This function uses cons to add the table in the db.
 * Insert:
 Search the table, checks the arguments and insert the record.
 * Update:
 This function searches the record and analyses the parameters. This was a hard one to implement because of the argument analyzer.
 
# Project Final Status

Current features: add tables, insert records, update records, remove records and tables.
Features in process: Proc and references.

# Student's Activity Log
# TimeSheet

**Roberto Bonilla**

| Activity                    | Description                                                                           | Time(h)  |
|:---------------------------:|:---------------------------------------------------------------------------:|:--------:|
| Investigation         | Lecture: José Helo, Introducción a la Programación con Scheme      	 | 8        |
| Writing code         | functions to analyze                   				         | 2        |  
| Investigation         | Eval function     	 								 | 3        |
| Writing code         | stored procedures           		 					 | 3        |
| Writing code         | relations                              		 					 | 3        |
| Debugging             | evaluation of stored procedures                              | 10     |
| Documentation   | functions                                                                                 | 3        |
| Documentation   | final details                                                                            | 2        |

**Pablo Rodriguez**

Duration  | Activity | Description
------------- | ------------- | -------------
6 h  | Investigation | Investigation of the language
3 h  | Coding | Programming the user interface 
2 h  | Planning | Meeting with Roberto
3 h  | Coding | Adding a table and advance in insert
2 h  | Coding | Finish insert and debugging
1 h  | Plannig | Skype with Roberto
5 h  | Coding | Programming the update function
2 h  | Coding | Programing remove functions
4 h  | Debug | Fixing problem with cproc eval
2 h  | Coding | Query by rows
# Conclusions
* The functional paradigm is very inefficient for common programming problems. It is recommended to use an imperative paradigm
for memory and cpu efficient programs
* The parenthesis of LISP are very annoying and it is recommended to watch every parenthesis and if you are familiar with c syntax, you probably 
will need to check if you used that syntax.
* The recursion is a new way of thinking and it is widely used in functional languages. But it improves the way you think in other ways rather than the usual.

# References
* "Artanis Home." <i>Learn Scheme in 15 Minutes</i>. N.p., n.d. Web. 21 Aug. 2015. http://web-artanis.com/scheme.html
* "Prototype to Polish: Making Games in CHICKEN Scheme with Hypergiant." Prototype to Polish: Making Games in CHICKEN Scheme with Hypergiant. N.p., n.d. Web. 21 Aug. 2015.  http://alex-charlton.com/posts/Prototype_to_polish_Making_games_in_CHICKEN_Scheme_with_Hypergiant/
* Scheme Tutorial." Scheme Tutorial. N.p., n.d. Web. 21 Aug. 2015.  https://classes.soe.ucsc.edu/cmps112/Spring03/languages/scheme/SchemeTutorialA.html#example
* "3.9 Local Binding: Let, Let*, Letrec, ..." 3.9 Local Binding: Let, Let*, Letrec, ... N.p., n.d. Web. 21 Aug. 2015. http://docs.racket-lang.org/reference/let.html
* "Scheme Macro Examples." Scheme Macro Examples. N.p., n.d. Web. 21 Aug. 2015. http://c2.com/cgi/wiki?SchemeMacroExamples
* "4.3 Strings." 4.3 Strings. N.p., n.d. Web. 21 Aug. 2015. http://docs.racket-lang.org/reference/strings.html
