P00100.Info <- data.frame(Id = "P00100", Title = "Police Approval", 
  FullText = "SENTIMENT - Taking everything into account, how good do you think the police in your area are in controlling crime? Do you think they do a good job or not?", 
  Min = -1, Max = 1, marks = I(list(c(-1,0,1))),
  labels = I(list(c("Disapprove","","Approve"))))

S0020.Info <- data.frame(Id = "S0020", Title = "Safe At Night", 
  FullText = "SENTIMENT - How safe do you feel walking alone in your area after dark? Do you feel very safe, fairly safe, a bit unsafe, or very unsafe?", 
  Min = 1, Max = 4, marks = I(list(c(1,2,3,4))), 
  labels = I(list(c("Very unsafe","Bit unsafe","Fairly safe","Very safe"))))

S0040.Info <- data.frame(Id = "S0040", Title = "Chance of Break-In", 
  FullText = "SENTIMENT - What would you say are the chances that over the next twelve months someone will try to break into your home? Do you think this is very likely, likely or not likely?", 
  Min = 1, Max = 3, marks = I(list(c(1,2,3))), 
  labels = I(list(c("Very likely","Likely","Not likely"))))

C01B400.Info <- data.frame(Id = "C01B400", Title = "Car Theft", 
                         FullText = "REPORT TO POLICE - Over the past five years have you or other members of your household had any of their cars/vans/trucks stolen? If so, did you or anyone else report the incident to the police?", 
                         Min = 0, Max = 1, marks = I(list(c(0,0.5,1))), 
                         labels = I(list(c("0% Reported","50%  Reported", "100%  Reported"))))

C03B400.Info <- data.frame(Id = "C03B400", Title = "Car Vandalism", 
                           FullText = "REPORT TO POLICE - Apart from thefts, have parts of any of the cars/vans/trucks belonging to your household been deliberately damaged (vandalized) over the past five years? If so, did you or anyone else report the incident to the police?", 
                           Min = 0, Max = 1, marks = I(list(c(0,0.5,1))), 
                           labels = I(list(c("0% Reported","50%  Reported", "100%  Reported"))))

C04B400.Info <- data.frame(Id = "C04B400", Title = "Theft of Motorcycle", 
                           FullText = "REPORT TO POLICE -  the past five years have you or other members of your household had any of their mopeds/scooters/motorcycles stolen? If so, did you or anyone else report the incident to the police?", 
                           Min = 0, Max = 1, marks = I(list(c(0,0.5,1))), 
                           labels = I(list(c("0% Reported","50%  Reported", "100%  Reported"))))

C06B400.Info <- data.frame(Id = "C06B400", Title = "Burglary", 
                           FullText = "REPORT TO POLICE - Over the past five years, did anyone actually get into your house or flat without permission and steal or try to steal something? If so, did you or anyone else report the incident to the police?", 
                           Min = 0, Max = 1, marks = I(list(c(0,0.5,1))), 
                           labels = I(list(c("0% Reported","50%  Reported", "100%  Reported"))))

C09B400.Info <- data.frame(Id = "C09B400", Title = "Robbery", 
                           FullText = "REPORT TO POLICE - Over the past five years has anyone taken something from you, by using force, or threatening you? Or did anyone try do to so? If so, did you or anyone else report the incident to the police?", 
                           Min = 0, Max = 1, marks = I(list(c(0,0.5,1))), 
                           labels = I(list(c("0% Reported","50%  Reported", "100%  Reported"))))

C11B400.Info <- data.frame(Id = "C11B400", Title = "Sexual Offenses", 
                           FullText = "REPORT TO POLICE - People sometimes grab, touch or assault others for sexual reasons in a really offensive way. This can happen either at home or elsewhere, for instance in a pub, the street, at school, on public transport, in cinemas, on the beach or at one's workplace. Over the past five years has anyone done this to you? If so, did you or anyone else report the incident to the police?", 
                           Min = 0, Max = 1, marks = I(list(c(0,0.5,1))), 
                           labels = I(list(c("0% Reported","50%  Reported", "100%  Reported"))))

C12B400.Info <- data.frame(Id = "C12B400", Title = "Assault", 
                           FullText = "REPORT TO POLICE - Apart from the incidents just covered, have you over the past five years been personally attacked or threatened by someone in a way that really frightened you either at home or elsewhere, such as in a pub, in the street, at school, on public transport, on the beach, or at your workplace? If so, did you or anyone else report the incident to the police?", 
                           Min = 0, Max = 1, marks = I(list(c(0,0.5,1))), 
                           labels = I(list(c("0% Reported","50%  Reported", "100%  Reported"))))

C13B400.Info <- data.frame(Id = "C13B400", Title = "Consumer Fraud", 
                           FullText = "REPORT TO POLICE - Last year, were you the victim of a consumer fraud? In other words, has someone when selling something to you or delivering a service cheated you in terms of quantity or quality of the goods/service? If so, did you or anyone else report the incident to the police?", 
                           Min = 0, Max = 1, marks = I(list(c(0,0.5,1))), 
                           labels = I(list(c("0% Reported","50%  Reported", "100%  Reported"))))

C02B400.Info <- data.frame(Id = "C02B400", Title = "Theft from Car", 
                           FullText = "REPORT TO POLICE - Over the past five years have you or have members of your household been the victim of a theft of a car radio, or something else which was left in your car, or theft of a part of the car, such as a car mirror or wheel? If so, did you or anyone else report the incident to the police?", 
                           Min = 0, Max = 1, marks = I(list(c(0,0.5,1))), 
                           labels = I(list(c("0% Reported","50%  Reported", "100%  Reported"))))

C05B400.Info <- data.frame(Id = "C05B400", Title = "Bicycle Theft", 
                           FullText = "REPORT TO POLICE - Over the past five years have you or other members of your household had any of their bicycles stolen? If so, did you or anyone else report the incident to the police?", 
                           Min = 0, Max = 1, marks = I(list(c(0,0.5,1))), 
                           labels = I(list(c("0% Reported","50%  Reported", "100%  Reported"))))


qtrans


QuestionMap <- rbind(P00100.Info, S0020.Info, S0040.Info, C11B400.Info, C09B400.Info, C06B400.Info, C04B400.Info, 
                     C03B400.Info, C01B400.Info, C12B400.Info, C13B400.Info, C05B400.Info, C02B400.Info)

setwd('~/Documents/Learning/NYC-DSA/Project 2/policingShiny')
saveRDS(QuestionMap, "datasets/QuestionMap.RDS")
