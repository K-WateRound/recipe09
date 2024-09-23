# ------------------------------------------------------------------------------------------------
# 번  호: 1
# 함수명: result_model_fn
# 입력값: model.name(모델명), actual(실제값), pred(예측값) 
# 설  명: df에 대한 평가지표 출력(MAE, MAPE, MSE, RMSE)
# ------------------------------------------------------------------------------------------------

result_model_fn <- function(model.name, actual, pred){    
    mae <- round(mean(abs(actual - pred)), 3)
    mape <- round(mean(abs((actual[actual != 0]-pred[actual != 0])/actual[actual != 0])) * 100, 3)
    mse <- round(mean((actual - pred)^2), 3)
    rmse <- round(sqrt(mse), 3)
   
    result_df <- data.frame(MODEL = model.name, MAE = mae, MAPE = mape, MSE = mse, RMSE = rmse)
    return(result_df)
}


# ------------------------------------------------------------------------------------------------
# 번  호: 2
# 함수명: result_plot_save_fn
# 입력값: model.name(모델명), actual(실제값), pred(예측값)
#        df(해당 모델에 대한 평가지표 데이터프레임), evaluation(평가지표)
# 설  명: 실제값과 예측값 비교 그래프를 png로 저장
# ------------------------------------------------------------------------------------------------

result_plot_save_fn <- function(model.name, actual, pred, df, evaluation){    
    # 실제값과 예측값 비교 그래프 저장(png)
    png(paste0('output/', model.name, '_plot.png'))  # png그래픽 디바이스 열기
    options(repr.plot.width = 7, repr.plot.height = 7)  # 그래프 크기 설정
    plot(pred, actual, xlab = 'Predicted', ylab = 'Actual',
         main = paste0('Actual and Predicted Output for ', model.name, '\n(', evaluation, '=', toString(df[, evaluation]), ')'))
    abline(a = 0, b = 1, col = 'red')
    dev.off()  # 그래픽 디바이스 닫기(파일로 저장)
}
