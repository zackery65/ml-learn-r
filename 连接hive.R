#windows以管理员身份打开命令行运行: R CMD javareconf, Mac/Linux运行sudo R CMD javareconf
library("DBI")
library("rJava")
library("RJDBC")

path = '/Users/gawain/src/java/hive_2.1.0/target/' #替换为你下载的hive依赖包的解压路径
hive = 'jdbc:hive2://hdp1.bi.bj2.yongche.com:10000/yc_ods'

.jinit(parameters=c("-DsocksProxyHost=IP", "-DsocksProxyPort=port"))

.jaddClassPath(dir(path,full.names=TRUE))
driver <- JDBC("org.apache.hive.jdbc.HiveDriver", paste(path, "/hive-jdbc-2.1.0.jar"))
conn <- dbConnect(driver, hive, "<username>", "<password>") #替换你的用户名密码


#测试
sql = "
    select end_time, count(1) cnt
    from (
        select service_order_id, create_time, from_unixtime(end_time, 'yyyyMMdd') end_time,
        row_number() over(partition by service_order_id order by update_time desc) num
        from fo_service_order
        where dt>=20161020 and dt <= 20161029 and status = 7
        and end_time >= unix_timestamp('2016-10-20 00:00:00')
        and end_time < unix_timestamp('2016-10-30 00:00:00')
    ) t
    where t.num = 1
    group by t.end_time
"
orders<- dbGetQuery(conn, sql)
