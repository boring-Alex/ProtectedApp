SpecTypesTable<-data.frame(Name = c("Гнои",
                                    "Мокрота и БАЛ",
                                    "Кровь",
                                    "Желчь",
                                    "Ликвор",
                                    "Моча",
                                    "Микроскопия на туберкулёз",
                                    "Секционный материал",
                                    "Дисбиоз",
                                    "УПФ",
                                    "Дизгруппа",
                                    "Диффициле",
                                    "Носительство коринебактерий",
                                    "Стафилококковое носительство"),
                           MaxNum = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))

DishesTable<-data.frame(Name = c("Кровяной агар",
                                 "Шоколадный агар",
                                 "Эндо",
                                 "Сабуро",
                                 "ЖСА",
                                 "Уриселект",
                                 "Энтерокккагар",
                                 "XLD-агар",
                                 "SS-агар",
                                 "Тиогликолевая среда",
                                 "Сывороточный агар",
                                 "Коринебак агар",
                                 "Агар Шадлера",
                                 "Хромогенный агар",
                                 "Престон агар"
                                 ))
if(Sys.info()["sysname"] == "Linux"){
  DishPerTypeTable<-read.csv("~/ShinyProjects/ProtectedApp/DataBases/TypesAndDishes.csv", dec=",")
}else{
  DishPerTypeTable<-read.csv("E:/LabApps/ProtectedApp/DataBases/TypesAndDishes.csv", dec=",")
}



BactAcceptedTable<-data.frame(AxaptaCode = character(),
                              SampleCode = character(),
                              AccDate = numeric(),
                              Type = character(),
                              CurrentNum = numeric(),
                              IsCompleted = logical())

SerolAcceptedTable<-data.frame(AxaptaCode = character(),
                              SampleCode = character(),
                              AccDate = numeric(),
                              Type = character(),
                              CurrentNum = numeric(),
                              IsCompleted = logical(),
                              StackNum = numeric())