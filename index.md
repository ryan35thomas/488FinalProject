# 488FinalProject
---
title: "Basketball's Offensive Evolution"
author: "Ryan Thomas & Michael Dara"
date: "7/6/2021"
output: 
  html_document:
    theme: cosmo
    toc: true
    toc_float: true
---


```{r, include=FALSE, echo=FALSE}
library(tidyverse)
library(tidymodels)
library(ggplot2)
cbb <- read.csv("C:/Users/ryan3/Documents/MATH 488P/10_eda/data/cbb.csv")
colnames(cbb)
str(cbb)
head(cbb)
```

## Introduction 

 In the last couple of years, basketball viewers know that the game has turned out to be all about offense and scoring. This means that players are aiming to score way more, the tempo is faster, and the 3-point shot is the best type of shot now. From an overarching view, it can be seen that basketball has shifted into a “European style” of play. European style basketball is considered team oriented fundamental basketball, which focuses on high basketball IQ, fundamentals, team spacing and tough defense. This style is more and more fitting as years  have gone by, because players’ individual skill sets have improved drastically. In turn, players are expected to have all essential skills on the court, such as shooting, rebounding, and passing, rather than choosing players based on one specific strength they have. 
  
## Background Info  
  
 This trend is most prevalent in the NBA, and in recent years, the three-point barrage has reached an all time high. According to the Wall Street Journal, “Twenty years ago, 17% of NBA shots were threes. Ten years ago, it was 22%. Five years ago, it climbed to 28%. And it didn’t stop there. The 3-point rate across the entire league kept rising until it soared near another milestone in the first month of this season: It’s settling at almost exactly 40%”. Furthermore, now there are 15 teams (half of the league) that are shooting the ball at this rate! (https://www.wsj.com/articles/nba-3-point-revolution-11611190418)
  
  Seeing NBA players like Stephen Curry, who shoots three-pointers 10 feet behind the line, has influenced the younger generations to do the same. 
  
## Project Description and Analysis
  
 So, to dive deeper into this phenomenon, our group has decided to investigate whether or not this offensive trend trickles down into the NCAA level. In doing so, we can determine whether other basketball leagues have also adopted this European style of play. This analysis is important for all of basketball, because women basketball players and their respective leagues can benefit the most, in terms of viewership and increasing revenue. Finally, it influences the younger generations into following the same style with their own teams. 
  
  When starting our project, we needed to acquire a dataset that best represented the last couple of seasons in NCAA Men’s Basketball. With the help of Andrew Sundberg, a Kaggle user, we were able to import a dataset of all the teams’ statistics in Men’s Division 1 Basketball from 2013-2019. For those who are not familiar with Kaggle, it is essentially a website that has many datasets covering any kind of topic or subject you can think of. After researching our acquired dataset, we decided to tidy it up because there were too many teams and conferences to deal with. From a dataset of about 2,500 rows, we were able to narrow it down to a little over 500 rows. We did it by extracting the top 6 conferences in the NCAA, since these conferences contain more influential players like NBA prospects and coaches that have been well-known for years. In doing so, we focused our sample on teams that are heavily watched and more comparable amongst each other, reducing the variability in our statistical modeling. The 6 conferences we selected are as follows: Atlantic Coast Conference (ACC), Big Ten (B10), Big Twelve (B12), Big East (BE), Pac-12 (P12), and Southeastern Conference (SEC). 

After extracting the necessary data from our set, we were able to properly view the variables associated with it. The main numerical variables that we analyzed were: adjusted tempo, 3-point shooting rate, free-throw rate, turnover-rate, effective field-goal rate, and wins-above-bubble rate (WAB). For those who are not aware, WAB is the number of games won above the bubble, the bubble being the cutoff for teams who do and don’t make the NCAA national tournament. Using these numerical variables, we were able to compare our categorical variables, which were conference, postseason seed, and year. 
  
  Once we determined all the possible variables to use, it was time to start formulating models to prove our point that the NCAA too has moved towards a European style of play. Our first major hypothesis was that a higher effective 3-point shooting rate would lead to a higher WAB, and in creating that, we made a scatterplot between those two variables. In order to better understand our plot, we separated the points on that plot by using color to determine postseason achievement.  In this context, postseason achievement is the place a team finished in the NCAA national tournament, with NA being teams that didn’t make the tournament. 
 

```{r, echo=FALSE}
cbb %>% select(X3P_O,POSTSEASON, WAB) %>% 
  ggplot()+ geom_point(aes(x= X3P_O, y=WAB, color= POSTSEASON)) +ggtitle("3 point % and Postseason Acheivment")
```

  From the above plot, it is clear that 3-point shooting rate is mightily important in winning. The mean of the shooting rate is at about 35 percent, but the teams with a rate higher than that go above and beyond in terms of postseason achievement, with most of them making it to the round of 32 or higher. It can be noted that there are sub-par shooting teams that also make the tournament, but of all those teams, only 1 out of the 7 listed champions shot below that average rate. Obviously, shooting 3 pointers is not the only aspect to the game, as rebounding, free throws, and 2-pointers are also crucial to winning championships, but the point trying to be conveyed here is that 3-point shooting is now usually the most important aspect in winning basketball games.  If your team cannot shoot 3’s, it is very likely that you will lose most games, and you will lose them at an even greater deficit than previous years. Our reason for saying this is that tempo has also increased as the years have gone by. Tempo is the statistic measuring the time it takes one team to hold the ball during one possession. With a faster tempo, better shooting teams are shooting more 3’s, accumulating more points altogether. One prime example of this is the 2015-2016 Golden State Warriors, whose bread and butter was their 3-point European style of basketball. They played at an extremely high tempo, and at the same time, shot 3’s any chance they got. It also helps that they have 2 of the top shooters ever to play in the sport. Therefore, this team was blowing out its competition, and many teams since then have tried to adopt that style of play. 
  
  On the other hand, however, critics may complain that this increased tempo leads to more turnovers than scoring opportunities. Their additional arguments for this style of play is that it is a bad adaptation for amateur level basketball players. NBA players are not comparable to NCAA players, as there is a large talent disparity between the two, however, our research on the NCAA in recent years shows that these players are still able to control the flow of the game with an increase in tempo. To refute their argument, we devised a plot showing the relationship between tempo and turnover rate. 
  
```{r, echo=FALSE, warning=FALSE}
cbb %>% select(TOR, ADJ_T) %>% ggplot() +
  geom_point(aes(x=ADJ_T, y= TOR)) + geom_smooth(aes(x=ADJ_T, y= TOR))+ggtitle("The Relationship of Tempo and Turnovers")
```

  From the plot, it can be communicated that a higher adjusted tempo does not necessarily mean an increase in the turnover rate. As the tempo increases, there is a slight increase in turnover rate, but then the turnover rate steadily declines. Our highest turnover rate is associated with a measly tempo rate of 64, opposed to the highest tempo being 80, so it is fair to see that the critics are wrong in justifying that increased tempo leads to higher turnover rate. Also, teams with higher tempo’s may just be superior teams in terms of protecting the basketball, as it can be seen that they turn the ball over less than teams that have a lower tempo rate. However, that cannot conclude that NCAA teams with higher tempo’s are better altogether. 
  
```{r, echo=FALSE}
cbb  %>% select(POSTSEASON, ADJ_T, X3P_O ) %>% 
  ggplot()+ geom_point(aes(x=ADJ_T, y= X3P_O , color= POSTSEASON)) + ggtitle("3 point % and Tempo Correlation")
```

  As you can see above, comparing 3 point rates and tempo rates amongst postseason achievement displayed a very weak positive correlation between the two variables, and it was indicative that postseason achievement didn’t have much to do with tempo. However, from our previous research, it is pretty 	clear that a team’s 3-point rate is vital in adopting the European style of play. 
  
## Conclusion 

  After thorough modeling and research with our given dataset, we have come to the conclusion that the NCAA has *slightly* adopted the NBA notion of playing a “European style” of basketball. The main reason for it only “slightly” adopting the style is due to the fact that there is a large talent gap between the NBA and lower level basketball leagues. NBA players shoot at a much higher rate than most NCAA athletes, so percentages don’t always line up with the way you would expect them to. In addition, other factors such as coaching and referees plays a part in allowing players to shoot more threes or play at a faster tempo. One big difference between the two is that the NCAA has a 30 second shot clock, opposed to the NBA’s 24 second shot clock, meaning that tempo will be lower on the NCAA side. On the bright side though, our modeling has shown us that players and teams are still trying to incorporate this style of play into their system. Our one grouped histogram on displaying tempo rates over the years has shown a steady increase, which is a good sign for the future. Finally, in the grand scheme of things, now that shooting has become more of an attraction than dunking and and-1’s, female leagues such as the WNBA and women’s NCAA can take advantage of this new style and brand it as their own, ultimately leading to more viewership and increased revenue. This is why we researched the adoption of European styled play into the NCAA. 
  
  
  ---
title: "Process_notebook"
author: "Ryan Thomas & Michael Dara"
date: "7/6/2021"
output: 
  html_document:
    theme: cosmo
    toc: true
    toc_float: true
---

We must load all the data and packages into R
```{r, include=FALSE,echo=FALSE}
library(tidyverse)
library(tidymodels)
library(ggplot2)
cbb <- read.csv("C:/Users/ryan3/Documents/MATH 488P/10_eda/data/cbb.csv")
colnames(cbb)
str(cbb)
head(cbb)
```

## Abstract
Basketball as a whole has seen a shift in the way the game is played. It is perceived as having the ability to play fast, and shooting threes on offense is a major key to having a successful offense in this day and age. This trend is very noticeable in the professional level, but one can wonder how this style has affected the play in the NCAA. By looking at data collected from 2013-2019 from teams of each of the major 6 conferences in the NCAA, we will look at how their tempo of play and 3 point shooting affected their win totals and overall finishes in the March Madness tournament. From this, our group created several graphics in an attempt to answer questions related to the data collected, in the hopes of bringing light to the effect that three point shooting and tempo can have on an NCAA game. 

## Overview and Motivation
The plan for this project is to see the effect that 3 point shooting and faster tempo can have on achievement in the 6 major conferences of Division 1 NCAA Men's basketball. The motivation behind this is the change of style in basketball, primarily from the NBA. Is it a good form of team building, should scouts be looking for players they can mold into so they can fit this more fast-paced and 3 point shooting orientated system?

## Related Work
Professional basketball over the last few years has seen a big style shift in the way the games have been played, as we read in the WSJ article: https://www.wsj.com/articles/nba-3-point-revolution-11611190418. The NBA has seen a change in the amount of 3 pointers shot and the overall tempo the games have been played at. This more run and gun "European Style" of basketball was pioneered in the NBA by the Houston Rockets, but has spread across the whole NBA. The era of running your offense mainly through your power-foward or center with his back to the basket in a slow paced half court setting was over. NBA teams began looking for players with a more well rounded game, more specifically, they wanted all 5 players on the floor to be able to shoot 3 pointers at an at least acceptable efficiency, while also contributing to other aspects of the game. This development in the professional leagues develops an obvious question, how is this affecting the college game? NCAA Men's basketball has several key rules difference as well as strategic differences when compared to the NBA. For one, the NCAA has a longer duration shotclock (30 seconds), than the NBA's(24 seconds), for those who don't know, the shotclock is the maximum amount of time the team has to attempt to shoot the ball and it resets once the ball hits the rim or possession of the ball is changed. Another major difference in rules is that the 3 point line is closer to the rim in the NCAA than in the NBA, which makes it easier for players to hit 3 pointers in the NCAA. And one key strategic difference in the NCAA than the NBA is the difference in talent when looking at team to team. These rule differences definitely have an effect on how the game is played. However, the NCAA game almost always is heavily influenced by the way NBA teams are playing. So, we would expect a shift into a similar style of play. This shift in style in the NBA is what made us curious is there a similar shift in the NCAA, and how effective is it?

## Data
Dataset origin: https://www.kaggle.com/andrewsundberg/college-basketball-dataset
This data set, which was named "cbb" in this analysis, had many interesting variables that pertained to the dataset. The ones used during our analysis were: CONF(Conference), W(Wins), EFG_O(Effective Field Goal%), TOR(Turnover rate), FTR(Free throw rate), 2P_O(2 point%), 3P_O (3 point%), ADJ_T(Adjusted Tempo), YEAR(Year), and POSTSEASON(March Madness finish). We did not believe much needed to be changed form the data because we wanted to display how the change in philosophy has changed over time as well as overall effectiveness regardless of time. One change we did make was we excluded the smaller conferences from the dataset, because a lot of these other divisions that were not included are so untalented that no matter how they are constructed they will not be successful to the bigger picture of the analysis. So, we decided to keep it to the 6 major conferences (SEC, PAC12, PAC10, BIG12, BIG10, BIG EAST).This data was then stored as cbb.csv and imported over to R.

## Initial Questions 

1. How has the teams’ tempo changed? Are team’s trying to play faster or slower than before?
  + This evolved into looking at how the tempo's changed within each conference over time which was then able to be displayed graphically, via a bar chart faceted by conference.

2. Is there a correlation between offensive rating and 3 point percentage? (Inverse or direct relationship?)
  + From this data we saw a strong relationship between offensive efficiency and three point shooting percentage. This caused us to wonder how much does 3 point percentage effect winning games. 
  
3. How does adjusted tempo and 3 point percentage affect the March Madness results for NCAA Teams?
  + This data helped show us how effective 3 point percentage was at bringing teams deeper to the playoffs then tempo was, but both had a general positive effect on having a deeper playoff run. This led us to question how else we can display the importance of three point shooting.

4. How has the turnover rate changed, and looking more closely, is this correlated to increased tempo?
  + This question arose to counter the claim that increasing tempo will then increase a teams turnover rate. From the data we collected, we did not see a relationship disproving the assumption that teams playing at a faster pace will then be more likely to turn the ball over. 

5. Are teams with a higher seed in the NCAA tourney rewarded more free throws compared to their opponents?
  + This question eventually evolved to looking on a per conference rate of free throws taken over time for each conference. Increases can be attributed to an increase in more aggressive and fast pace play which can lead to more fouls and free throw attempts. To go deeper, a further question can look to compare tempo and freethrow attempts.

6. Are primary 3-point shooting teams more or less likely to win the NCAA tourney than teams that shoot more 2’s?
  + The data did not offer us attempts but instead percentages, so we looked to compare three point and 2 point efficiency on team success which was measured by how they finished in the March madness tournament. 

7. Is there a correlation between a team’s Wins above bubble (WAB) and their postseason achievement?
  + This question eventually evolved to WAB and 3-point% on postseason achievement, which was measured by how the team finished in the March Madness tournament. 

8. How has shooting percentages changed over time? Is this due to an increased volume of shot attempts, and how has the distribution of shots changed? 
  + This question was eventually scarped and we wanted to see if there is a linear regression that exists between 3 point percentage and tempo on team wins. 

## Exploratory Data Analysis

#### Question 1
```{r, echo=FALSE}
cbb %>% group_by(CONF) %>% ggplot()+
  geom_bar(aes(x = YEAR, y = ADJ_T, fill = CONF), position = "dodge", stat = "identity")+ 
  coord_cartesian(ylim = c(55,80))+
  facet_wrap(~CONF)
```

For question 1, we created a bar chart where the year was the x value and y is the Adjusted tempo, and then the data was faceted by Conference. The bar chart does a good job of showing how the tempo has changed over time. The reason it was chosen is because of the type of data we had we could not use a scatterplot with year. The reason we decided to facet by conference was to show how each conference has changed over time since each conference tends to have a different style of play it was only fair to split it by conference. From this we saw that most conferences saw a somewhat increasing amount of tempo over time.

#### Question 2
```{r, echo=FALSE}
cbb  %>% select(TEAM, EFG_O, X3P_O ) %>%  
  ggplot()+ geom_point(aes(x=EFG_O, y= X3P_O )) + geom_smooth(aes(x=EFG_O, y= X3P_O ))
```

For #2, we created a scatter plot to answer the question of how 3-point% affected effective field-goal%, and we found a positive linear relationship between the two variables. We used a scatter plot as it did the best job of showing the relationship between these two variables. This data was useful in establishing the importance of 3-point% on the offense's efficiency. 

#### Question 3
```{r, echo=FALSE}
cbb  %>% select(POSTSEASON, ADJ_T, X3P_O ) %>% 
  ggplot()+ geom_point(aes(x=ADJ_T, y= X3P_O , color= POSTSEASON))
```

For #3, we created a scatterplot where the x axis corresponded to the adjusted tempo, the y axis was corresponding to the 3-point%, and used color to represent the results of the teams march madness finish. This information was important to displaying the importance of 3-point shooting and tempo and the graph from this was actually chosen to be directly used in our blog post. We can conclude that there is a weakly positive association between 3-point% and adjusted tempo. From this, we also see that it seemed that 3-point% had more of an effect on the teams post season success than adjusted tempo. This was concluded because of how there was still colors resulting in some form of post season success in the higher 3-point % area (over 35%), regardless of tempo. But having a combination of both tempo and a high 3 point % lead to even more post season success. 

#### Question 4
```{r, echo=FALSE}
cbb %>% select(TOR, ADJ_T) %>% ggplot() +
  geom_point(aes(x=ADJ_T, y= TOR)) + geom_smooth(aes(x=ADJ_T, y= TOR))+ggtitle("The Relationship of Tempo and Turnovers")
```

This next graph was also very important to the dataset because it was able to disprove a counterclaim to playing with a higher tempo. From this scatterplot, we see that there is no relationship between tempo and turnovers, as the data seems to be randomly spread out around the graph. This is useful in showing that playing fast will not necessarily lead to more turnovers if done properly. Other factors such as poor coaching or low skilled players can be to blame for the amount of turnovers seen from some of the high tempo teams. 

#### Question 5
```{r, echo=FALSE}
cbb %>% group_by(CONF) %>% 
  ggplot()+
  geom_point(aes(x = CONF, y = FTR, color = CONF))+
  facet_wrap(~YEAR)
```

This scatter plot helped display how each conferences free-throw rate changed over time, in hopes to tying it into how an increase in tempo lead to more free throws, but the data shows otherwise. We see that free throw rates have actually decreased over time which upon further analysis makes sense because of an increase in three pointers attempted. In basketball most fouls take place very close to the basket it is less frequent that you see fouls being called out by the three point line. so with an increase in 3 pointers, we can see why there is a decrease in freethrows. In addition, the slight increase in tempo accounts for less time holding the ball, which gives the referees less time to call fouls. 

#### Question 6
```{r, echo=FALSE}
cbb  %>% select(POSTSEASON, X2P_O, X3P_O ) %>% 
  ggplot()+ geom_point(aes(x=X3P_O, y= X2P_O , color= POSTSEASON))
```

This data set was used to display a relationship between 3 point and 2 point percentages on post season achievement. There is a a positive linear relationship between 3 point and 2 point percentages represented in the scatterplot. We see thanks to the colors that for the most part, shooting better from 3 point is going to lead to slightly more post season success then just shooting well from 2. But being able to shoot well form both ranges lead to the greatest team success. 

#### Question 7
```{r, echo=FALSE}
cbb %>% select(X3P_O,POSTSEASON, WAB) %>% 
  ggplot()+ geom_point(aes(x= X3P_O, y=WAB, color= POSTSEASON)) +ggtitle("3 point % and NCAA March Madness results ")
```

We created a scatterplot where we compared the x-axis of 3 point percentage on the y-axis of wins above the bubble(WAB), and then use color again to represent post season success. We see from this graph that there is a positive linear relationship between WAB and 3-point%, displaying that many of the good 3 point shooting teams win a lot more games then those who do not shoot as well from three. Then wee see that most of these teams also ended up going on to have pretty good post season success which we see represented by the different colors.

#### Question 8
```{r, echo=FALSE}
lm1 <- (lm(W ~ X3P_O + ADJ_T, data = cbb))
summary(lm1)
plot(lm1)
```

For this final point we attempted to try and create a linear regression model, in which we tried to use adjusted tempo(ADJ_t) and 3-point% to predict regular season wins. The data we found was inconclusive because our t test caused us to fail to reject the null hypothesis. The reasoning why we believe this test failed is that we are missing any defensive metrics to predict wins, which is needed to fully explain a team's wins. One way in which we can further our research hypothetically is trying to use both of those explanatory variables to predict total points in a season scored by a each team, each year.We could not conduct this test because we don't have access to such data. 

## Final Analysis

This data gave us a good indication that the NCAA has somewhat adopted the NBA's new form of basketball as there has seemed to be a reliance on being able to shoot well from the 3 point area. There seems to be less of an emphasis on tempo than in the NBA, which there are several explanations for. For one, there is an obvious difference in talent gap in the NBA compared to the NCAA, which lowers how effectively a team can play with a higher tempo. Thus, at the college level, there is a slow down. In addition, the longer shotclock in the NCAA encourages longer possessions, which will then lower a teams adjusted tempo. But, we did see a relationship between teams that were skilled enough to shoot the three efficiently and play with a high tempo and their ability to do well in the regular season and post season, which is justified by the scatter plots from questions 3, 5 and 7.  
