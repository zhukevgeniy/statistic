Sys.setlocale(category = "LC_ALL", locale = "Russian")


df <- read.table('./data/input.txt', header=FALSE, sep = ',')

# отсортированный дата фрэйм
sorted_df <- df[order(df)]

# вектор значений 
vector_df <- as.double(sorted_df)

# вариациооный р€д
variational_series <- as.data.frame(table(vector_df))

min_df <- min(vector_df)

max_df <- max(vector_df)

# размах выборки
sample_r <- max_df - min_df

# выборочное среднее
sample_mean <- rowMeans(df)

df_length <- length(df)

# выборочна€ дисперси€
sample_var <- var(vector_df) * (df_length - 1) / df_length

# исправленна€ дисперси€
population_variance <- sd(vector_df)

# медиана
median_df <- median(vector_df)

# нижн€€ квартиль
quantile_low <- unname(quantile(vector_df, 1/4))

# верхн€€ квартиль
quantile_high <- unname(quantile(vector_df, 3/4))

# квантиль пор€дка 1/3
quatile_3 = unname(quantile(vector_df, 1/3))

# гистограмма 1
hist(vector_df, probability = TRUE, col="gray")

lines(table(vector_df), col="pink", lty="dotted")

lines(dnorm(vector_df,mean=median_df, sd=population_variance), col="red")

# гистограмма 2
plot(ecdf(vector_df))
lines(density(vector_df), col="green")



      
     