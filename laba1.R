df <- read.table('./input.txt', header=FALSE, sep = ',')

#отсортированный датасет
sorted_df <- df[order(df)]

#вектор значений
vector_df <- as.double(sorted_df)

#вариационный ряд
variational_series <- as.data.frame(table(vector_df))

min_df <- min(vector_df)

max_df <- max(vector_df)

#размах выборки
sample_r <- max_df - min_df

#выборочное среднее
sample_mean <- rowMeans(df)

df_length <- length(df)

#выборочная дисперсия
sample_var <- var(vector_df) * (df_length - 1) / df_length

#исправленная выборочная дисперсия
population_variance <- sd(vector_df)

#медиана
median_df <- median(vector_df)

#нижняя квартиль
quantile_low <- unname(quantile(vector_df, 1/4))

#верхняя квартиль
quantile_high <- unname(quantile(vector_df, 3/4))

#выборочная кванитиль порядка 1/3
quatile_3 = unname(quantile(vector_df, 1/3))

#гистограмма 1
hist(vector_df, probability = TRUE, col="gray")

lines(table(vector_df), col="pink", lty="dotted")

lines(dnorm(vector_df,mean=median_df, sd=population_variance), col="red")

#гистограмма 2
plot(ecdf(vector_df))
lines(density(vector_df), col="green")



      
     