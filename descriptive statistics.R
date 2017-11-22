#####################################################################################
# Step 1 
#  data for the last 1 year for a set of the five stocks 
#####################################################################################
# First date = 2015-08-03, Last date = 2016-08-01

print("===== Download 5 stocks =====")
download_data = function(url) {
  raw = read.table(url, header=TRUE, sep=",")
  raw = raw[, c(1, 7)] # 1 is Date column, 7 is Adj.Close column
  raw = raw[nrow(raw):1, ] # Sort oldest to newest
  return(raw)
}

msft_url = "http://chart.finance.yahoo.com/table.csv?s=MSFT&a=7&b=1&c=2015&d=7&e=1&f=2016&g=m&ignore=.csv"
fb_url = "http://chart.finance.yahoo.com/table.csv?s=FB&a=7&b=1&c=2015&d=7&e=1&f=2016&g=m&ignore=.csv"
goog_url = "http://chart.finance.yahoo.com/table.csv?s=GOOG&a=7&b=1&c=2015&d=7&e=1&f=2016&g=m&ignore=.csv"
twrbe_url= "http://chart.finance.yahoo.com/table.csv?s=TWR.BE&a=7&b=1&c=2015&d=7&e=1&f=2016&g=m&ignore=.csv"
amzn_url = "http://chart.finance.yahoo.com/table.csv?s=AMZN&a=7&b=1&c=2015&d=7&e=1&f=2016&g=m&ignore=.csv"

"MSFT - Microsoft"
msft = download_data(msft_url)
msft
"FB - Facebook"
fb = download_data(fb_url)
fb
"GOOG - Alphabet (Google)"
goog = download_data(goog_url)
goog
"TWR.BE - Twitter"
twrbe = download_data(twrbe_url)
twrbe
"AMZN - Amazon"
amzn = download_data(amzn_url)
amzn

print("")
print("")
#####################################################################################
# Step 2
# Calculate Monthly returns of downloaded stock over the period under study
#####################################################################################
print("===== Monthly Returns of each stock =====")
monthly_returns = function(data) {
  close_values = data[, 2] # 2 is Adj.Close column
  num = length(close_values) - 1
  returns = numeric(num)
  for (i in 1:num) {
    returns[i] = close_values[i + 1] - close_values[i]
  }
  return(returns)
}

"MSFT Monthly Returns"
msft_returns = monthly_returns(msft)
msft_returns
"FB Monthly Returns"
fb_returns = monthly_returns(fb)
fb_returns
"GOOG Monthly Returns"
goog_returns = monthly_returns(goog)
goog_returns
"TWR.BE Monthly Returns"
twrbe_returns = monthly_returns(twrbe)
twrbe_returns
"AMZN Monthly Returns"
amzn_returns = monthly_returns(amzn)
amzn_returns

print("")
print("")
#####################################################################################
# Step 3
# Using a combination function, calculate the monthly returns of 
# an equally weighted portfolio consisting of any 3 of the five stocks in question
#####################################################################################
print("===== Average Monthly Returns of 3 in 5 stocks =====")
cases = combn(c(1,2,3,4,5), 3)

color = rainbow(length(cases) / 3)
plot(0, 0, xlim = c(1,13), ylim = c(0,150),
        xlab = "Month", ylab = "Cumulative Monthly Return", type = "n")

variance_sum = 0
all_avg_monthly_returns = NULL

num_points = length(msft_returns)
for (i in 1:(length(cases)/3)) {
  avg_returns = numeric(num_points)
  for (j in 1:num_points) {
    avg_returns[j] = 0
    for (k in 1: 3) {
      ret = 0
      quote = cases[k, i]
      if (quote == 1) {
        ret = msft_returns[j]
      } else if (quote == 2) {
        ret = fb_returns[j]
      } else if (quote == 3) {
        ret = goog_returns[j]
      } else if (quote == 4) {
        ret = twrbe_returns[j]
      } else if (quote == 5) {
        ret = amzn_returns[j]
      }
      avg_returns[j] = avg_returns[j] + ret
    }
    avg_returns[j] = avg_returns[j] / 3
    all_avg_monthly_returns = append(all_avg_monthly_returns, avg_returns[j])
  }
  label = ""
  for (k in 1:3) {
    quote = cases[k, i]
    if (quote == 1) {
      label = paste(label, "MSFT", sep=" ") 
    } else if (quote == 2) {
      label = paste(label, "FB", sep=" ") 
    } else if (quote == 3) {
      label = paste(label, "GOOG", sep=" ") 
    } else if (quote == 4) {
      label = paste(label, "TWR.BE", sep=" ") 
    } else if (quote == 5) {
      label = paste(label, "AMZN", sep=" ") 
    }
  }
  print(label)
  print(avg_returns)
#####################################################################################
# Step 4
# Graphically represent the cumulative monthly returns of 
# each of the possible portfolios through line plots
#####################################################################################
  cum_of_avg_returns = numeric(num_points)
  cum_of_avg_returns[1] = avg_returns[1]
  for (j in 2:num_points) {
    cum_of_avg_returns[j] = cum_of_avg_returns[j - 1] + avg_returns[j]
  }
  lines(cum_of_avg_returns, col = color[i])
#####################################################################################
# Step 5
# Calculate mean, median and standard deviation of monthly values for each of
# the portfolios in question and plot them on the same graph mentioned in step 4.
#####################################################################################
  mean_val = mean(avg_returns)
  median_val = median(avg_returns)
  sd_val = sd(avg_returns)
  text(10, cum_of_avg_returns[12], 
    paste(
      label, 
      'Monthy return stats:',
      'Mean = ', format(round(mean_val, 2), nsmall = 2),
      'Median = ', format(round(median_val, 2), nsmall = 2),
      'SD = ', format(round(sd_val, 2), nsmall = 2)
    ),
    cex=0.5)
  variance_sum = variance_sum + (sd_val ** 2)
}

print("")
print("")
#####################################################################################
# Step 6
# Calculate the overall variance of all portfolio returns
#####################################################################################
print("===== Overall variance  =====")
overall_variance = variance_sum / (length(cases) / 3)
overall_variance