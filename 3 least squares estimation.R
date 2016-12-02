# computing correlation between normalized galton's data
cor(gpa_nor, gch_nor)

l_nor <- lm(gch_nor ~ gpa_nor)
