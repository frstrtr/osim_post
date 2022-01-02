Требования:
убедиться что установлен OpenSim, R и добавляены в PATH для вызова из командной строки
убедиться что в R установлены все нужные пакеты
###############################
# install.packages("R.utils")
# install.packages("logging")
# install.packages("XML")
# install.packages("methods")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("scales")
###############################

------
Шаги для воспроизведения
<изначальное получение данных из OpenSim'a>
1. пойти в файл jump_CMC_post_processing.r и поменять путь на нужный (переменная path)
2. запустить из командной строки файл run_all_CMC_normalizations_serial.bat
3. дождаться выполнения
<получение промежуточного отчета от Жени>
4. скопировать .mot файлы в аналогичные без цифровой версии в каталог прыжка, т.е. например jump_jo_2.mot -> jump_jo\jump_jo.mot
5. скопировать ext_forces файлы в аналогичные без цифровой версии в каталог прыжка, т.е. например jump_jo_2_ext_forces.mot -> jump_jo\jump_jo_ext_forces.mot
6. пойти в файл report_script.r и поменять путь на нужный (строка 15)
7. пойти в файл report_script.r и уточнить список рассматриваемых прыжков (строка 22) 
8. запустить report_script.bat и дождаться выполнения
<получение сводной таблицы по мышцам>
9. идем в файл jump_CMC_aggregate.r и меняем путь на нужный (строка 20)
9. идем в файл jump_CMC_aggregate.r и меняем список прыжков на нужный (нужно оставить только те, для которых выполнился CMC. например у нас он не выполнился для jo500, его убираем) строка 27 
10. запустить jump_CMC_aggregate.bat и дождаться выполнения
11. будет создан файл aggregated_CMC_muscles_table.csv - это один из итоговых редультатов
<получение итоговой таблицы и графиков перенос энергии>
12. удалить лишние строчки из Report.csv (там где NA)
13. скопировать файлы _powers.csv из отдельных каталогов в корневой, jump_jo\jump_jo_powers.csv -> jump_jo_powers.csv
14. идем в файл jump_CMC_muscle_power_and_energy_transfer.r и меняем путь на нужный (строка 20)
15. идем в файл jump_CMC_muscle_power_and_energy_transfer.r и меняем список прыжков на нужный (нужно оставить только те, для которых выполнился CMC. например у нас он не выполнился для jo500, его убираем) строка 38 
16. запустить jump_CMC_muscle_power_and_energy_transfer.bat и дождаться выполнения
17. получились финальные картинки в виде .png файлов
18. получилась табличка aggregated_Final_Report.csv
19. получилась табличка Energy_transfer.csv

Итоговый набор:
Report.csv и более читаемая версия aggregated_Final_Report.csv
aggregated_CMC_muscles_table.csv
Energy_transfer.csv
картинки .png
