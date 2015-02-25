# Create calendar files
library(lubridate)
library(dplyr)
library(stringr)
generate.dates <- function(first,last.before.midterm,first.after.midterm,last,holidays,location,weekdays,start.time,duration,type="Lecture") {
  dates <- c(first+c(0:(last.before.midterm-first))*days(1),first.after.midterm+c(0:(last-first.after.midterm))*days(1))
  dates <- dates[wday(dates,label=TRUE,abbr=FALSE) %in% weekdays ]
  dates <- dates[!(dates %in% holidays)]
  hour(dates) <- hour(start.time)
  minute(dates) <- minute(start.time)
  end.dates <- dates + duration
  data.frame(type=type,weekday=wday(dates,label=TRUE,abbr=FALSE), start.date=dates,end.date=end.dates,location=location)
}

first.teaching.date <- dmy("3-Mar-2015", tz=Sys.timezone()) 
last.teaching.date.before.midterm <- dmy("31-Mar-2015", tz=Sys.timezone()) 
first.teaching.date.after.midterm <- dmy("21-Apr-2015", tz=Sys.timezone()) 
last.teaching.date <- dmy("5-Jun-2015", tz=Sys.timezone()) 
lecture.location <- "New Kirk KKLT301 (T) / Maclaurin MCLT103 (F)" 
lecture.weekdays <- c("Tuesday","Friday") 
lecture.duration <- new_duration(minute=50) 
holidays <- c(dmy("3-Apr-2015","27-Apr-2015","1-Jun-2015")) # Good Friday, Anzac day, Queen's Birthday

start.time <- hm("14:10")
lecture.dates <- generate.dates(first.teaching.date,last.teaching.date.before.midterm,first.teaching.date.after.midterm,last.teaching.date,holidays,lecture.location,lecture.weekdays,start.time,lecture.duration,type="Lecture")
write.csv(lecture.dates,"lecture.dates.csv")
#Google fields: Subject,Start Date,Start Time,End Date,End Time,All Day Event,Description,Location,Private

deadlines <- data.frame(type = "RCP deadline", weekday = NA, start.date = ymd_hm(c("2015-3-13 17:00","2015-4-24 23:59","2015-5-1 23:59","2015-5-22 23:59","2015-6-5 23:59"), tz = Sys.timezone()), end.date = NA, location = "Online", Description = c("Vote to rank questions for RCP","Post Tutorial Proposal","Do self-assessment","Complete classification","Complete essay")) %>% mutate(end.date = start.date + hours(1), weekday = wday(start.date,label=TRUE,abbr=FALSE))

discussion.board.deadlines <- c(first.teaching.date+days(11)+hours(17),first.teaching.date+days(25)+hours(17),first.teaching.date+days(39)+hours(17),first.teaching.date.after.midterm+days(11)+hours(17),first.teaching.date.after.midterm+days(25)+hours(17),first.teaching.date.after.midterm+days(39)+hours(17))

google.calendar.file <- rbind(google.calendar.file, data.frame(start.date=search.strategies.report.start.date,end.date=search.strategies.report.end.date,location="Tutorial",Subject="Search Strategies Report",Description="Hand in your search strategies report at your tutorial this week"))

google.calendar.file <- rbind(google.calendar.file, data.frame(start.date=essay.1.deadline,end.date=essay.1.deadline+hours(1),location="Electronically on Blackboard",Subject="Essay 1", Description="Deadline for Essay 1. See topics on Blackboard. Only an electronic copy is needed, unless your tutor requests otherwise."))

google.calendar.file <- rbind(google.calendar.file, data.frame(start.date=essay.2.deadline,end.date=essay.2.deadline+hours(1),location="Electronically on Blackboard",Subject="Essay 2",Description="Deadline for Essay 2. See topics on Blackboard. Only an electronic copy is needed, unless your tutor requests otherwise."))

google.calendar.file <- rbind(google.calendar.file, data.frame(start.date=discussion.board.deadlines,end.date=discussion.board.deadlines+hours(1),location="Electronically on Blackboard",Subject="Discussion board post deadline", Description="Deadline for Discussion Board Post. Post before 5pm."))

#Google fields: Subject,Start Date,Start Time,End Date,End Time,All Day Event,Description,Location,Private
google.calendar.file$start.time <- ldply(str_split(google.calendar.file$start.date," "))[,2]

google.calendar.file$start.date <- ldply(str_split(google.calendar.file$start.date," "))[,1]

google.calendar.file$end.time <- ldply(str_split(google.calendar.file$end.date," "))[,2]

google.calendar.file$end.date <- ldply(str_split(google.calendar.file$end.date," "))[,1]

google.calendar.file <- google.calendar.file[ ,c("Subject","start.date","start.time","end.date","end.time","Description","location")]

names(google.calendar.file) <- c("Subject","Start Date","Start Time","End Date","End Time","Description","Location")

write.csv(google.calendar.file,"google.calendar.file.csv",row.names=FALSE)

lecture.location <- "Hunter HULT 119" 
lecture.weekdays <- c("Tuesday","Thursday") 
start.time <- hm("15:10")
lecture.dates.261 <- generate.dates(first.teaching.date,last.teaching.date.before.midterm,first.teaching.date.after.midterm,last.teaching.date,holidays,lecture.location,lecture.weekdays,start.time,lecture.duration,type="Lecture")
lecture.dates.261

lecture.topics.261 <- c("Introduction (no reading)", "Thucydides","Thucydides","Plato","Plato","Plato/Aristotle","Aristotle","Aristotle")

lecture.readings.261 <- c("Introduction (no reading).","Ancient Conceptions of Politics. Thucydides, 1.21-3, 2.34-46 (Funeral Oration), 2.50-54 (The Plague), 3.36-49 (The Mytilenian Debate), 5.84-116 (The Melian Dialogue).","Thucydides continued.","Plato, Laws, Books 1-2.","Plato, Laws, Book 3","Aristotle, Politics, Book 1 (1252a1-1260a25).","Aristotle, Politics, Book 3, chapters 1-18 (1274b30-1288b7).","Aristotle, Politics, Book 7, chapters 1-15 (1323a14-1334b27)","Just War in Medieval Political Theory. Aquinas, Summa Theologiae, II-II, Q 40; II-II, Q 64, articles 6-8.","Vitoria, On the American Indians (selections); Erasmus, Dulce Bellum Inexpertis.","New Conceptions of Politics. Machiavelli, The Prince, chapters 1-3, 5-6, 8-9, 15-19, 21, 25.","Machiavelli, The Prince discussion continued and Discourses on Livy, I.2-14","Machiavelli, Discourses on Livy, I.16-18, 58, II.1","The State of Nature. Hobbes, Leviathan, chapters 13-14, sections from chapter 15, 17-18, 21.","Rousseau, Second Discourse on the Origins of Inequality, part I.","Rousseau, Second Discourse on the Origins of Inequality, part II.","Democracy, Intervention, and Progress. Kant, Perpetual Peace: A Philosophical Sketch, entire.","Hegel, The Philosophy of Right, III.iii.a.i-III.iii.c (paragraphs 321-360).","Marx and Engels, Communist Manifesto, entire.","Mill, Considerations on Representative Government, chapters 3-4.","Mill, Considerations on Representative Government, chapters 6, 8.","Mill, Considerations on Representative Government, chapters 16 and 18, Mill, “A Few Words on Non-Intervention.","Kant, Hegel, Mill and Marx compared. Revision.","Revision.")

lecture.dates.261 <- cbind(lecture.dates.261,lecture.readings.261)
write.csv(lecture.dates.261,"lecture.dates.261.csv")
