### TidyTuesday data - 2022-05-10 - made by Lorenzo Amabili ###

# Loading R libraries
library(readr)
library(tidyverse)

# Loading the data
nyt_titles = read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full = read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# Checking the data
head(nyt_titles)
head(nyt_full)

# Considering only the weeks in which a book was ranked as first
nyt_full_first = nyt_full[which(nyt_full$rank==1), ]
head(nyt_full_first)

# Cleaning the data
View(unique(nyt_full_first["author"]))
nyt_full_first["author"][nyt_full_first["author"] == "? by Jimmy Buffett"] = "Jimmy Buffett"
nyt_full_first["author"][nyt_full_first["author"] == "A.J. Cronin"] = "A. J. Cronin"
nyt_full_first["author"][nyt_full_first["author"] == "C.J. Box"] = "C. J. Box"
nyt_full_first["author"][nyt_full_first["author"] == "adapted by Joan D. Vinge"] = "Joan D. Vinge"
nyt_full_first["author"][nyt_full_first["author"] == "Daphne du Maurier"] = "Daphne Du Maurier"
nyt_full_first["author"][nyt_full_first["author"] == "Dean R. Koontz"] = "Dean Koontz"
nyt_full_first["author"][nyt_full_first["author"] == "F. Van Wyck Mason"] = "F. van Wyck Mason"
nyt_full_first["author"][nyt_full_first["author"] == "written and illustrated by Mattie J. T. Stepanek"] = "Mattie J. T. Stepanek"
nyt_full_first["author"][nyt_full_first["author"] == "created by Bill Adler and written by Thomas Chastain"] = "Bill Adler and Thomas Chastain"
nyt_full_first["author"][nyt_full_first["author"] == "edited by Joyce Reardon"] = "Joyce Reardon"
nyt_full_first["author"][nyt_full_first["author"] == "George R.R. Martin"] = "George R. R. Martin"
nyt_full_first["author"][nyt_full_first["author"] == "J. R. R. Tolkien. Edited by Christopher Tolkien. Illustrated by Alan Lee"] = "J. R. R. Tolkien"
nyt_full_first["author"][nyt_full_first["author"] == "J.R.R. Tolkien"] = "J. R. R. Tolkien"
nyt_full_first["author"][nyt_full_first["author"] == "James Gould Cozzens"] = "James G. Cozzens"
nyt_full_first["author"][nyt_full_first["author"] == "James Michener."] = "James Michener"
nyt_full_first["author"][nyt_full_first["author"] == "John Phillips Marquand"] = "John P. Marquand"
nyt_full_first["author"][nyt_full_first["author"] == "Scott Turow."] = "Scott Turow"
nyt_full_first["author"][nyt_full_first["author"] == "Samuel Shellaburger"] = "Samuel Shellabarger"
nyt_full_first["author"][nyt_full_first["author"] == "W. E.B. Griffin"] = "W. E. B. Griffin"
nyt_full_first["author"][nyt_full_first["author"] == "James Michener"] = "James A. Michener"
nyt_full_first["author"][nyt_full_first["author"] == "John le Carré"] = "John Le Carré"
nyt_full_first["author"][nyt_full_first["author"] == "James Patterson with Andrew Gross"] = "James Patterson and Andrew Gross"
View(unique(nyt_full_first["author"]))


# Aggregating the data by book title and author
nyt_full_grouped = aggregate(nyt_full_first[,3], list(nyt_full_first$year, nyt_full_first$title, nyt_full_first$author), sum)
nyt_full_grouped_ordered = nyt_full_grouped[order(nyt_full_grouped$Group.1), ]
nyt_full_books = aggregate(nyt_full_grouped_ordered[,4], list(nyt_full_grouped_ordered$Group.2, nyt_full_grouped_ordered$Group.3), sum)
nyt_full_authors = aggregate(nyt_full_grouped_ordered[,4], list(nyt_full_grouped_ordered$Group.3), sum)

# Finding the top 10 books and authors
nyt_full_books = nyt_full_books[order(nyt_full_books$x, decreasing=TRUE), ]
nyt_full_books_10 = nyt_full_books[1:10, ]
nyt_full_authors = nyt_full_authors[order(nyt_full_authors$x, decreasing=TRUE), ]
nyt_full_authors_10 = nyt_full_authors[1:10, ]
colnames(nyt_full_books_10) = c("book_title", "author", "total_weeks")
colnames(nyt_full_authors_10) = c("author", "total_weeks")
nyt_full_books_10
nyt_full_authors_10

# Finding the related information of the top 10 books and authors
author10 = nyt_full_authors_10$author
book10 = nyt_full_books_10$book_title
year_book = aggregate(nyt_full_first[,1], list(nyt_full_first$title), min)
year_author = aggregate(nyt_full_first[,1], list(nyt_full_first$author), min)
year_author_10 = year_author[which(year_author$Group.1 %in% author10), ]
year_book_10 = year_book[which(year_book$Group.1 %in% book10), ]
colnames(year_book_10) = c("book_title", "year")
colnames(year_author_10) = c("author", "year")
final_top10_authors = merge(nyt_full_authors_10, year_author_10, by="author")
final_top10_books = merge(nyt_full_books_10, year_book_10, by="book_title")
final_top10_authors
final_top10_books
