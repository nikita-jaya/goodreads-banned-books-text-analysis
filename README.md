# Text Analysis for Goodreads Descriptions of Banned Books

*Completed as a final project for Carnegie Mellon University's 36668 Graduate Text Analysis course*

To quote Ray Bradbury’s “Fahrenheit 451”, “A book is a loaded gun in the house next door”
(Bradbury (1951)). Ironically, it seems that an increasing number of schools across the United
States has taken this literary advice to heart. PEN America keeps track of school book bans,
and they reported 4,231 unique titles that were banned between July 1, 2023 to June 30, 2024;
note that this encapsulates 10,046 instances of individual books banned across different schools
in the United States (PENAmerica (2024)). We will use only a subset of these books during
the same time period. This analysis is interested in comparing the Goodreads descriptions
for books that are “more banned” (i.e. banned in more than 1 state within a single year) and
books that are “less banned” (i.e. books banned in 1 state within a single year). In particular,
this analysis explored the following questions:

• How do the literary features differ between the Goodreads descriptions of more-banned
and less-banned books?

• What are the keywords that differ between the Goodreads descriptions of more-banned
and less-banned books?

• Do the frequencies of these keywords and/or the occurrence of these features differ based
on when they were banned within the year?

Using Goodreads descriptions rather than the full texts of the novels allows for easier ac-
cess (especially with more recent books that aren’t freely available). In addition, the use of
Goodreads descriptions rather than the actual books allows for a focus on a larger breadth
of banned books and is not restricted by copyright laws. Admittedly, this unfortunately par-
takes in another unfortunate logic highlighted in Fahrenheit 451, which is using summaries
of books rather than the full text of the book (Bradbury (1951)). However, it is easier to
assess topics of a book from the Goodreads description since these descriptions will mention
general narrative details more explicitly rather than the subtlety in the actual novel.
