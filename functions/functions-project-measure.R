

removeLargeDifferencesBetweenColumns<- function(df, df.col1, df.col2, max.diff)
    {
  
      
      #print(typeof(df.col1))
      for(row in 1:nrow(df))
      {
    
        if(!is.na(df[row, df.col1]) & !is.na(df[row, df.col2])) 
        {
          if(abs(df[row, df.col1]-df[row, df.col2]) > max.diff)
          {
            df[row, df.col1]=NA
            df[row, df.col2] = NA
          }
        }
      }
      #assign('df',df,envir=.GlobalEnv) #it stopped working and I didn't know what was wrong and I ended up changining a ton of things one at a time and ended up putting everything back to exactly how it was and it workd without this butI feel more comfortable keepingit in
      return(df) #return statement
    }



cleanNonNumericColumns <- function(df)
    {
      
      #change units 
      #colnames(masterdf)
      #table(masterdf["units"])
      
      #movies.50[which(movies.50$millionsAdj2000 < 1), "cluster.arbitrary"] = 2
      df[which(df$units== '"in"'), "units"]= 'in'
      df[which(df$units== 'Inch'), "units"]= 'in'
      df[which(df$units== 'inches'), "units"]= 'in'
      
      table(df$units)
      
      #change ethnicity
      #The official ethnic groups are American Indian or Alaska Native, Asian, Black or African American, Hispanic or Latino, Native Hawaiian or Other Pacific Islander, and white. These are the groups I will be using, I changed the different nationalities and races into the correct ethnicity group, and added another for 2 or more ethnicities. Note- ethnicity is not nationality, and it is a little subjective, just like how race has some subjectivity.
      
      #table(masterdf$ethnicity)
      
      #
      asianEthnicities = c('"asian"', 'Asain', 'asian', 'Asian', 'chinese', 'Filipino', 'indian', 'Japanese', 'korean', 'laotian')
      whiteEthnicities = c('"caucasian"', 'anglo', 'caucasain', 'caucasian', 'Caucasian', 'white', 'White', 'White Non-Hispanic', 'white italian')
      blackEthnicities = c('african american', 'African American', 'black')
      latinoHispanicEthnicities = c('hispanic', 'Hispanic', 'latin american', 'latino')
      nativeAmericanEthnicities = c('native american', 'Native American')
      twoOrMoreEthnicities = c('"asian/latino"', 'Caucasian/Asian', 'japanese italian', 'white-filipino')
      
      #go through each row and change the ethnicity to the correct value
      for(row in 1:nrow(df))
      {
        if(is.element(df[row, "ethnicity"], asianEthnicities)){
          df[row, "ethnicity"] = 'asian'
        }
        else if(is.element(df[row, "ethnicity"], whiteEthnicities)){
          df[row, "ethnicity"] = 'white'
        }
        else if(is.element(df[row, "ethnicity"], blackEthnicities)){
          df[row, "ethnicity"] = 'black'
        }
        else if(is.element(df[row, "ethnicity"], latinoHispanicEthnicities)){
          df[row, "ethnicity"] = 'hispanic or latino'
        }
        else if(is.element(df[row, "ethnicity"], nativeAmericanEthnicities)){
          df[row, "ethnicity"] = 'native american'
        }
        else if(is.element(df[row, "ethnicity"], twoOrMoreEthnicities)){
          df[row, "ethnicity"] = 'two or more ethnicities'
        }
        
      }
      table(df$ethnicity)
      
      #gender
      #For gender the responses receives were either male or female, and one non binary, so I am going to use the values M, F, and NB with the same method as for ethnicity
      
      #table(df$gender)
      femaleResponses = c('"female"', 'f', 'F', 'female', 'Female')
      maleResponses = c('"male"', 'male', 'm', 'M', 'Male')
      for(row in 1: nrow(df))
      {
        if(is.element(df[row, 'gender'], femaleResponses)){
          df[row, 'gender'] = 'F'
        }
        else if(is.element(df[row, 'gender'], maleResponses)){
          df[row, 'gender'] = 'M'
        }
        else
          df[row,'gender'] = 'NB'
      }
      table(df$gender)
      
      
      #side
      #side is good, there is left, right, and N/A, so there is nothing to clean
      #table(df$side)
      
      
      #eye_color
      #The colors are blue, brown, black, blue-green, blue-grey, brown, green, grey, grey-green, hazel, and left. I am making black part of brown, becasue black isn't a typical eye color and is really dark brown, and I am removing left, for obvious reasons
      
      table(df$eye_color)
      blue = c('"blue"', 'blue', 'Blue')
      brown = c('"brown"', 'Brown', 'brown', 'black')
      hazel = c('hazel', 'Hazel')
      green = c('"green"', 'green', 'Green')
      remove = c('left')
      blueGreen = c('blue-green', 'blue/green')
      
      
      for(row in 1: nrow(df))
      {
        df[row, 'eye_color']
        if(is.element(df[row, 'eye_color'], blue)){
          df[row, 'eye_color'] = 'blue'
        }
        else if(is.element(df[row, 'eye_color'], brown)){
          df[row, 'eye_color'] = 'brown'
        }
        else if(is.element(df[row, 'eye_color'], hazel)){
          df[row, 'eye_color'] = 'hazel'
        }
        else if(is.element(df[row, 'eye_color'], green)){
          df[row, 'eye_color'] = 'green'
        }
        else if(is.element(df[row, 'eye_color'], blueGreen)){
          df[row, 'eye_color'] = 'blue-green'
        }
        else if(is.element(df[row, 'eye_color'], remove)){
          df[row, 'eye_color'] = NA
        }
        
      }
      table(df$eye_color)
      
      #writing
      #table(df$writing)
      right = c('"right"', 'Right', 'right')
      left = c('left', 'Left')
      for(row in 1:nrow(df))
      {
        if(is.element(df[row, 'writing'], right))
          df[row, 'writing'] = 'right'
        else if (is.element(df[row, 'writing'], left))
          df[row, 'writing'] = 'left'
        
      }
      table(df$writing)
      
      
      #swinging
      #In swinging, there are some typos, where left is spelled "let" or "leftt, and right is spelled "rigth", I caught with table(), and fixed them
      right = c('"right"', 'Right', 'right', 'rigth')
      left = c('left', 'Left', 'leftt', 'let')
      for(row in 1:nrow(df))
      {
        if(is.element(df[row, 'swinging'], right))
          df[row, 'swinging'] = 'right'
        else if (is.element(df[row, 'swinging'], left))
          df[row, 'swinging'] = 'left'
      }
      table(df$swinging)
      
      #Later in the process I found some duplicated values so I went back and removed them to not confuse me or distort the data
      df[duplicated(df$person_id),]
      df[df$person_id== '1cef05bce7879e0ffee01b0cb8d78c32', ] #There are duplicates!
      library(dplyr)
      #df %>% distinct(person_id, .keep_all = TRUE) didn't work
      df[duplicated(df$person_id),]
      df = df[!duplicated(df$person_id), ] # this works
      
      
      return(df)
    }


convertUnits<-function(df)
    {
      #convert the measurements that are in inches to centimeters then change the label for these values
      
      
      library(measurements) #conv_unit() finction 
      table(df$units)#check which units need to be convertes
      df[which(df$units== 'in'), 1:26] #before to check if it woked
      
      df[which(df$units== 'in'), 4:26]= conv_unit(df[which(df$units== 'in'), 4:26], 'inch', 'cm')
      
      
      df[which(df$units== 'in'), 1:26] #after
      
      #change units to centimeters
      df[which(df$units == 'in'), 'units'] = 'cm'
      
      return(df)
    }


cleanHeight<-function(df)
    {
      
      #boxplots to check for outliers and possible errors- my plan is to ocmbine the left and right measurements into a single 
      df.numericOnly = df[,4:26] #makes a copy 
      df.numericOnly
      library(Hmisc)
      describe(df[,4:26])
      #There are a lot of really small values, the lowest 5  range from 27 to 93 centimeters (11 to 36 inches), I want to check whether these are babies or kids
      #df.numericOnly$age = df$age
      #plot(df$age, df$height.NA)
      #The three erroneous values look like the highest one, and the bottom two, I will check if they are outliers 
      #boxplot(df$height.NA)
      # 
      # dfchildren = df[which(df$age <15),]
      # describe(dfchildren$height.NA)
      # boxplot(dfchildren$height.NA)
      # plot(dfchildren$age, dfchildren$height.NA)
      # dfchildren
      # #In the children subset the lowest four heights were less than 100cm, which is about 39 inches, and the ages of these people are 1, 3, 7, and 9. I think the 3 year old may be valid, but for the other ages this is too small,and I am going to take them out
      #remove the four lowest heights and the highest one, which is 464 cm (15 feet)
      #There is one three year old whose height is 93 cm, which is very close to the average for three year olds, which is 94 cm. That is why I am going to keep it in my data as of right now, and when I am doing analysis if it is distorting my results I can compare my results with or without it 
      
      df[which(df$height.NA== 27.3050), 'height.NA'] = NA
      df[which(df$height.NA== 36), 'height.NA'] = NA
      df[which(df$height.NA== 85.090), 'height.NA'] = NA
      df[which(df$height.NA== 91.440), 'height.NA'] = NA
      df[which(df$height.NA > 400), 'height.NA']= NA
      
      # describe(df$height.NA)
      # plot( df$age ,df$height.NA)
      # boxplot(df$height.NA)
      
      return(df)
    }

cleanHeadCircumference <- function(df)
    {
      
      # hist(df$head.circumference.NA)
      # plot(df$height.NA, df$head.circumference.NA) # This shows me that the three lowest values are NOT from babies who SHOULD have a smaller head circumference and that I need to remove them
      # #I removed the values greater than 60 because that is the values over about two feet and what seems too big for a head
      # #The average values for head circumferences are 53 - 61 cm, so I am removing the values outside the range (52, 62) 
      # describe(df$head.circumference.NA)
      df[which(df$head.circumference.NA < 52), 'head.circumference.NA'] = NA
      df[which(df$head.circumference.NA >62), 'head.circumference.NA'] = NA
      #boxplot(df$head.circumference.NA)
      return(df)
    }

cleanHeadHeight <-function(df)
    {
      #hist(dfcopy$head.height.NA)
      #dfcopy = df # copy to test in case I mess up and want to go back
      #The first percentile for females head height is 17.2 cm and the 99th percentile for males for head height is 21.3 source(https://upload.wikimedia.org/wikipedia/commons/6/61/HeadAnthropometry.JPG), so I will discard all the values outside these values, and possibly be alittle more conservative to allow for imprecision in measurement, and will also check these against ages or height to check if the small values are from the  children
      #Since there are so many outliers and it is difficult to tell which is an erroneous value and by plotting it against age or heignt it isn't obvious again what the true outliers are, I am going to remove the ones that are outliers by using the interquartile method, where the values that are greater than Q3+1.5IQR or less than Q1-1.5IQR
      #dfcopy[which(dfcopy$head.height.NA>30), "head.height.NA"] = NA
      
      #dfcopy[which(dfcopy$head.height.NA<10), "head.height.NA"] = NA
      # plot(df$height.NA, df$head.height.NA)# x,y
      # plot(df$age, df$head.height.NA)# x,y
      # boxplot(df$head.height.NA)
      # summ = summary(df$head.height.NA)
      IQR = 24-21 #Q3-Q1
      min = 21-1.5 * IQR #min-1.5IQR
      max = 24 + 1.5 * IQR #max+1.5IQR
      df[which(df$head.height.NA>max), "head.height.NA"] = NA
      df[which(df$head.height.NA<min), "head.height.NA"] = NA
     # boxplot(df$head.height.NA)
      
      #hist(df$head.height.NA)
      
      return(df)
    }

cleanHands <- function(df)
    {
      #left
      # dfcopy = df
      # #check if any are wildly different and look at values
      # plot(dfcopy$hand.width.left, df$hand.width.right)
      # #one value is over fifty
      # hist(dfcopy$hand.width.left)
      # hist(dfcopy$hand.width.right)
      # #check against height to see if the really small values are the children
      # summary(dfcopy$hand.width.left)
      # summary(dfcopy$hand.width.right)
      
      leftIQR =21.50 - 18
      leftmin = 18-1.5*leftIQR
      leftmax = 21.5+1.5*leftIQR
      
      rightIQR= 21.39-18.41
      rightmin = 18.41-1.5*rightIQR
      rightmax = 21.39+1.5*rightIQR
      df[which(df$hand.width.left< leftmin), "hand.width.left"] = NA
      df[which(df$hand.width.left>leftmax), "hand.width.left"] = NA
      df[which(df$hand.width.right< rightmin), "hand.width.right"] = NA
      df[which(df$hand.width.right>rightmax), "hand.width.right"] = NA
      
      
      
      # plot(dfcopy$age, dfcopy$hand.width.left) #the reallylow values are not children
      # plot(dfcopy$height.NA, dfcopy$hand.width.left) #they are not the really shortpeople either
      # 
      # #subjective cutoff for outliers
      # 
      # hist(dfcopy[which(dfcopy$hand.width.left<=17),"age"]) #histogram of all hands smaller than mine(I'm 5 feet tall and hand width 18.2 cm) to see if the small hands are mostly children 
      # 
      #hand length
      #plot(dfcopy$hand.length.left, dfcopy$hand.length.right)
      #There are three obvious outliers above 50 centimeters(over 19 inches)
      df[which(df$hand.length.left>50), "hand.length.left"] = NA
      df[which(df$hand.length.right>50), "hand.length.right"] = NA
      #I see three values under 10, I will check to see if these are children
      df[which(df$hand.length.left <10), 'age']
      df[which(df$hand.length.right <10), "age"] #21, 1, 67, 67, 67
      
      #check the 67 year olds
      df[which(df$age == 67),] #this person's height is missing but all the measurements are very small, so this person must be very short so I will keep ip
      
      
      return(df)
    }

cleanElbowArmpit <-function(df)
    {
      #dfcopy = df
      #plot(df$elbow.armpit.left, df$elbow.armpit.right) #there are two observations where the difference between the left and right is substantial, I will remove those observations because there must be a mistake
      df = removeLargeDifferencesBetweenColumns(df, "elbow.armpit.left", "elbow.armpit.right", 2.54)
      
      
      #boxplots to check for outliers
      # boxplot(df$elbow.armpit.left)
      # boxplot(df$elbow.armpit.right)
      #remove outliers
      q1left = quantile(df$elbow.armpit.left, .25, na.rm = TRUE)
      q3left = quantile(df$elbow.armpit.left, .75, na.rm = TRUE)
      iqrleft = q3left - q1left
      minleft = q1left - 1.5*iqrleft
      maxleft = q3left + 1.5*iqrleft
      
      q1right = quantile(df$elbow.armpit.right, .25, na.rm = TRUE)
      q3right = quantile(df$elbow.armpit.right, .75, na.rm = TRUE)
      iqrright = q3right - q1right
      minright = q1right - 1.5*iqrright
      maxright = q3right + 1.5*iqrright
      
      df[which(df$elbow.armpit.left<minleft), "elbow.armpit.left"] = NA
      df[which(df$elbow.armpit.left>maxleft), "elbow.armpit.left"] = NA
      df[which(df$elbow.armpit.right<minright), "elbow.armpit.right"] = NA
      df[which(df$elbow.armpit.right>maxright), "elbow.armpit.right"] = NA
      
      #plot(df$height.NA, df$elbow.armpit.left) #looks pretty good, no visible outliers
      return(df)
    }

cleanArmSpan <-function(df)
    {
      # describe(df$arm.span.NA) #lowest values ttange from 8-75 cm
      # plot(df$height, df$arm.span.NA) #There are a couple reallylow outliers
      # 
      q1 = 158.5
      q3 = 179.4
      iqr = q3 - q1
      min = q1 - 1.5*iqr
      max = q3 + 1.5*iqr
      
      df[which(df$arm.span.NA < min), "arm.span.NA"] = NA
      df[which(df$arm.span.NA>max), "arm.span.NA"] = NA
      
      # describe(df$arm.span.NA) #lowest values ttange from 8-75 cm
      # plot(df$height.NA, df$arm.span.NA)
      return(df)
    }


cleanArmReach <-function(df)
    {
      
      # plot(df$arm.reach.left, df$arm.reach.right)
      # boxplot(df$arm.reach.left)
      # boxplot(df$arm.reach.right)  #there are several low outliers, I want to check if they are kids or short people
      # 
      # plot(df$height.NA, dfcopy$arm.reach.left) #There are several values that look too low
      # plot(df$age, dfcopy$arm.reach.left) #The lowest values are not from reallu short people. THey are from people with a height from 140 to 180, so I think that these are mismeasurements and willr emove them with the 1.5IQR method
      # 
      # summary(df$arm.reach.left)
      # summary(df$arm.reach.right)
      
      iqrleft = 221.4-192
      minleft = 192-1.5*iqrleft 
      maxleft = 221.4+1.5*iqrleft
      
      iqrright = 222.4-189.7
      minright = 189.7-1.5*iqrright
      maxright = 222.4+1.5*iqrright
      
      df[which(df$arm.reach.left< minleft), "arm.reach.left"]= NA
      df[which(df$arm.reach.left> maxleft), "arm.reach.left"]= NA
      
      df[which(df$arm.reach.right< minright), "arm.reach.right"]= NA
      df[which(df$arm.reach.right> maxright), "arm.reach.right"]= NA
      #it look sbetter, although there is still quite a spread in the data
      return(df)
    }

cleanHandElbow <-function(df)
    {
      #dfcopy = df
      # plot(df$hand.elbow.left, df$hand.elbow.right)
      # plot(df$height, df$hand.elbow.left) 
      # boxplot(df$hand.elbow.left)
      # boxplot(df$hand.elbow.right) #skews down
      # 
      # summary(df$hand.elbow.left)
      # summary(df$hand.elbow.right)
      leftiqr = 45.50 - 40
      leftmin= 40-1.5*leftiqr
      leftmax = 45.50+1.5*leftiqr
      rightiqr = 45.50 - 40
      rightmin = 40-1.5*rightiqr
      rightmax = 45.50+1.5*rightiqr
      
      df[which(df$hand.elbow.left<leftmin),"hand.elbow.left"] = NA
      df[which(df$hand.elbow.left>leftmax),"hand.elbow.left"] = NA
      df[which(df$hand.elbow.right<rightmin),"hand.elbow.right"] = NA
      df[which(df$hand.elbow.right>rightmax),"hand.elbow.right"] = NA
      #THis looks a lot better and the outliers ar cleaned up
      # hist(df$hand.elbow.left)
      # hist(df$hand.elbow.right)
      return(df)
    }


cleanFloorHip <-function(df)
    {
      #dfcopy = df
      # plot(df$floor.hip.left, df$floor.hip.right)
      df = removeLargeDifferencesBetweenColumns(df, 'floor.hip.left', 'floor.hip.right', 4)
      
      # boxplot(df$floor.hip.left) #There are several low outliers
      # describe(df$floor.hip.left)
      # describe(df$floor.hip.right)
      # plot(df$age, df$floor.hip.left)
      # plot(df$height.NA, df$floor.hip.right)
      # 
      #I am going to remove the bottom three values
      rmlist = c(24.5, 35, 46.5) #left and right
      
      df[which(df$floor.hip.left < 50), "floor.hip.left"] = NA #gets rid of three smallest values on each side
      df[which(df$floor.hip.right < 50), "floor.hip.right"] = NA
      return(df)
    }

cleanFloorKneepit <- function(df)
    {
      
      df = removeLargeDifferencesBetweenColumns(df, "floor.kneepit.left", "floor.kneepit.right", 3)
    
      # plot(df$height.NA, df$floor.kneepit.right) #again, one large person
      # summary(df$floor.kneepit.left)
      # 
      df[which(df$floor.kneepit.left> 70), ]
      #I found that there is one person with measurements that awa WAY higher than the rest, so I will remove that whole person 
      df = df[-which(df$person_id =='81a9f3915f64dc6edd329b89bea87d5e'),]
      # boxplot(df$floor.kneepit.left)
      # boxplot(df$floor.kneepit.right)
      # 
      # summary(df$floor.kneepit.left)
      # summary(df$floor.kneepit.right)
      # 
      #remove outliers
      q1left = quantile(df$floor.kneepit.left, .25, na.rm = TRUE)
      q3left = quantile(df$floor.kneepit.left, .75, na.rm = TRUE)
      iqrleft = q3left - q1left
      minleft = q1left - 1.5*iqrleft
      maxleft = q3left + 1.5*iqrleft
      
      q1right = quantile(df$floor.kneepit.right, .25, na.rm = TRUE)
      q3right = quantile(df$floor.kneepit.right, .75, na.rm = TRUE)
      iqrright = q3right - q1right
      minright = q1right - 1.5*iqrright
      maxright = q3right + 1.5*iqrright
      
      df[which(df$floor.kneepit.left<minleft), "floor.kneepit.left"] = NA
      df[which(df$floor.kneepit.left>65), "floor.kneepit.left"] = NA
      df[which(df$floor.kneepit.right<minright), "floor.kneepit.right"] = NA
      df[which(df$floor.kneepit.right>65), "floor.kneepit.right"] = NA #There is another person who's measurements don't fit with the height distribution and seems erroneous so I am removing it as well
      
      return(df)
    }


cleanFootLength <- function(df)
    {
     
      #dfcopy = df #copy in case I make mistakes
      #plot(df$foot.length.left, df$foot.length.right) #There are some values that are too different from each other to be plausible
      df = removeLargeDifferencesBetweenColumns(df,"foot.length.left", "foot.length.right", 2.54)
    
      
      #plot(df$foot.length.left, df$foot.length.right) #there is one value over 60cm (15 inches)
      df[which(df['foot.length.left']>50), "foot.length.left"] = NA
      df[which(df['foot.length.right']>50), "foot.length.right"] = NA
      
      #check the other foot lengths against height to see if they belong to tall/short people
      # plot(df$height.NA, df$foot.length.left) #I will remove the highest and the lowest because they don'e look like they ar correct
      # boxplot(df$foot.length.left)
      # summary(df$foot.length.left)
      df[which(df['foot.length.left']<=15), "foot.length.left"] = NA
      df[which(df['foot.length.right']<=15), "foot.length.right"] = NA
      df[which(df['foot.length.left']>=35), "foot.length.left"] = NA
      df[which(df['foot.length.right']>=35), "foot.length.right"] = NA
      
      return(df)
    }

cleanFloorNavel <-function(df)
    {
      # hist(df$floor.navel.NA)
      # plot(df$height.NA, df$floor.navel.NA)
      # plot(df$age, df$floor.navel.NA) #for the really low outliers it looks like they are from kids and that the high values are the true outliers
      # boxplot(df$floor.navel.NA)
      # summary(df$floor.navel.NA)
      
      iqr = 107-95
      min = 95-1.5*iqr
      max = 107+1.5*iqr
      df[which(df$floor.navel.NA <min), "floor.navel.NA"] = NA
      df[which(df$floor.navel.NA >max), "floor.navel.NA"] = NA
      
      #after running the hist and boxplots that I did above again, the visuals look better so I wont do anything else
      #describe(df$floor.navel.NA)
      return(df)
    }

cleanFloorArmpit<- function(df)
    {
      
      # plot(df$floor.armpit.left, df$floor.armpit.right)#check whether two sides are wildly different and correct it
      # boxplot(df$floor.armpit.left)
      # boxplot(df$floor.armpit.right)
      #print(dfcopy$floor.armpit.left-dfcopy$floor.armpit.right) #several measurements vary by 20 to 50 cm, need to remove those
      
      df = removeLargeDifferencesBetweenColumns(df, "floor.armpit.left", "floor.armpit.right", 3.81)
      
      # plot(df$floor.armpit.left, df$floor.armpit.right)
      # 
      # plot(df$age, df$floor.armpit.left) #some of the small values are children
      # plot(df$height, df$floor.armpit.left) #this looks like a good distribution, and nothing is way out of range, I will keep all these values
      # 
      return(df)
    }

cleanDataset <-function(df)
    {
  
  
  #very first:
  #measure.df = removeDuplicatesFromDataFrameAllColumns(measure.short);


  #check the ones where arm reach is less than height


  #note: add this- remove the people who gave bad data

     
      # 
      # df = removeDuplicatesFromDataFrameAllColumns(df);
      # df = subsetDataFrame(df, "data_collector", "!=", "411f8b5c9500b7cc928c93dd1a0006b7");
      # df = subsetDataFrame(df, "person_id", "!=", "81a9f3915f64dc6edd329b89bea87d5e");
      # df = subsetDataFrame(df, "person_id", "!=", "\"ee33e909372d935d190f4fcb2a92d542\"");
      # 
      #comparison = cm.df$arm.reach > cm.df$height; #check arm reach on na values
      #cm.ndf = cm.df[comparison,]; # keep "TRUE" rows, what about NA?
      
      df = cleanNonNumericColumns(df)
      df = convertUnits(df)
      df = cleanHeight(df)
      df = cleanHeadCircumference(df)
      df = cleanHeadHeight(df)
      df = cleanHands(df)
      df = cleanElbowArmpit(df)
      df = cleanArmSpan(df)
      df = cleanArmReach(df)
      df = cleanHandElbow(df)
      df = cleanFloorHip(df)
      df = cleanFootLength(df)
      df = cleanFloorNavel(df)
      df = cleanFloorKneepit(df)
      return(df)
}


combineLRColumns <-function(df, newcolName, leftCol, rightCol)
    {
      dfnew = df
      dfnew[newcolName] = NULL
      for(row in 1:nrow(df))
      {
        if(!is.na(df[row, rightCol]) &is.na(df[row, leftCol]))#right there, left missing
        {
          dfnew[row, leftCol] = df[row, rightCol]
        }
        else if(is.na(df[row, rightCol]) & !is.na(df[row, leftCol])) #left there, right missing
        {
          dfnew[row, newcolName] = df[row, leftCol]
        }
        
        else if(!is.na(df[row, rightCol]) & !is.na(df[row, leftCol])) #neither missing so take average
        {
          left = df[row, leftCol]
          right = df[row, rightCol]
          dfnew[row, newcolName] = (left + right)/2
        }#end elseif
      }#end for
      dfnew <- dfnew[,!names(dfnew) %in% c(leftCol,rightCol)]
      return(dfnew)
    }

combineAllColumns <-function(df)
    {
    dfnew = df
    dfnew = combineLRColumns(dfnew, "handLength", "hand.length.left", "hand.length.right")
    dfnew = combineLRColumns(dfnew, "floor.armpit","floor.armpit.left", "floor.armpit.right")
    dfnew = combineLRColumns(dfnew,"floorHip","floor.hip.left","floor.hip.right"  )
    dfnew = combineLRColumns(dfnew, "floorKneepit", "floor.kneepit.left", "floor.kneepit.right")
    dfnew = combineLRColumns(dfnew, "footLength",  "foot.length.left", "foot.length.right")
    dfnew = combineLRColumns(dfnew, "armReach", "arm.reach.left", "arm.reach.right")
    dfnew = combineLRColumns(dfnew, "handElbow", "hand.elbow.left", "hand.elbow.right")
    dfnew = combineLRColumns(dfnew, "handWidth", "hand.width.left", "hand.width.right")
    dfnew = combineLRColumns(dfnew, "elbowArmpit","elbow.armpit.left", "elbow.armpit.right")
    
      return(dfnew)
    }
