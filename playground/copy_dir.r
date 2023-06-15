# 実行 Rscript main.r
# 特定のフォルダ配下の、該当するフォルダとその中のファイルを見つけてcopyする
# 該当するフォルダの中のフォルダまでは探索できない..

# R library you have to install by yourself
library(magrittr)
library(purrr)


# default R library
# なし


path <- "~/Documents" # root folder to search eg. ~/Documents
if (!dir.exists(path)) {
    stop("フォルダみつからない")
}


result_path <- sprintf("%s/result", path) # folder to store results
# なければ作成
if (!dir.exists(result_path)) {
    dir.create(result_path)
}

# もしdirなら、createして再帰, file ならcopy のような処理にしたかったけど、
# めんどくさそうだったから保留
copy_dir <- function(from, to) {
    # if (!file_test("-d", dir_path)) {
    #     # もしフォルダでなければ、exception
    #     msg <- sprintf("%sはフォルダでないです.\n", dir_path)
    #     stop(msg)
    # }
    # if (dir.exists(to)) {
    #     # もし既にフォルダが存在するなら、exception
    #     msg <- sprintf("%sフォルダは既に存在してます.\n", to)
    #     stop(msg)
    # }
}

# applying tail iteratively through the strsplit list, taking the 1st element of the tail-end of each vector.
# The 1 is telling tail how many elements to take. The default is six, which is what you were probably getting

target_dir_pattern <- "spark" # 探索したいフォルダ名. 正規表現でもok
list.files(path, recursive = TRUE, include.dirs = TRUE, pattern = target_dir_pattern) |>
    map(trimws) |>
    map(\(x) c(sapply(strsplit(x, "/"), tail, 1), x)) |>
    unique() |>
    walk(\(dir) {
        copy_to <- sprintf("%s/%s", result_path, dir[1])
        copy_from <- sprintf("%s/%s", path, dir[2])
        # なければ作成
        if (!dir.exists(copy_to)) {
            dir.create(copy_to, recursive = TRUE)
        }

        # フォルダ配下の全ファイルをcopy
        # ここで include.dirs=FALSE にしてもフォルダ名を拾ってくるのはなぜなんだろう.
        list.files(copy_from, include.dirs = FALSE) |> walk(\(y) {
            z <- sprintf("%s/%s", copy_from, y) # 絶対path
            if (file_test("-f", z)) {
                file.copy(from = z, to = sprintf("%s/%s", copy_to, y))
            }
        })
    })

cat("done.")
