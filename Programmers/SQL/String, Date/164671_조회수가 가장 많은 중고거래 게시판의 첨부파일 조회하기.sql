select b.file_path
from USED_GOODS_BOARD as a join
    (SELECT board_id, file_id
            , concat("/home/grep/src/",board_id,"/",file_id,file_name,file_ext) as file_path
    from used_goods_file) as b
on a.board_id=b.board_id
where a.views in 
(select max(views) from used_goods_board)
order by b.file_id desc