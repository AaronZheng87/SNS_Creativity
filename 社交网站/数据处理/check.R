df2 %>% 
  select(SNS_t, SES_t, CSES_t, EIB_t) %>% 
  var()
df2 %>% 
  select(SNS_t, SES_t, CSES_t, EIB_t) 


df_check <- df1 %>%
  mutate(
    SNS_t = (SNS1 + SNS2 + SNS3 + 
               SNS4 + SNS5 + SNS6) / 6,
    SES_t = (SES1 + SES2 + SES3_r + 
      SES4 + SES5_r + SES6 + SES7 +
      SES8 + SES9_r + SES10_r)/10,
    CSES_t = (CSES1 + CSES2 + 
      CSES3 + CSES4)/4,
    EIB_t = (EIB1 + EIB2 + EIB3 + 
      EIB4 + EIB5 +
      EIB6 + EIB7 + EIB8)/8
  ) %>%
  dplyr::select(
    SNS_t, SES_t,
    CSES_t, EIB_t
  )

report::report(df_check)


df_check2 <- df1 %>%
  mutate(
    SNS_t = (SNS1 + SNS2 + SNS3 + 
               SNS4 + SNS5 + SNS6) / 6,
    SES_t = (SES1 + SES2 + SES3_r + 
               SES4 + SES5_r + SES6 + SES7 +
               SES8 + SES9_r + SES10_r)/10,
    CSES_t = (CSES1 + CSES2 + 
                CSES3 + CSES4)/4,
    EIB_t = (EIB1 + EIB2 + EIB3 + 
               EIB4 + EIB5 +
               EIB6 + EIB7 + EIB8)/8
  ) %>%
  dplyr::select(
    SNS_t, SES_t,
    CSES_t, EIB_t
  )


?scale



df_check2 <- df1 %>%
  mutate(
      SNS_t = (SNS1 + SNS2 + SNS3 + 
                 SNS4 + SNS5 + SNS6) / 6,
      SES_t = SES1 + SES2 + SES3_r + 
        SES4 + SES5_r + SES6 + SES7 +
        SES8 + SES9_r + SES10_r,
      CSES_t = CSES1 + CSES2 + 
        CSES3 + CSES4,
      EIB_t = EIB1 + EIB2 + EIB3 + 
        EIB4 + EIB5 +
        EIB6 + EIB7 + EIB8,
      s_SES = scale(SES_t, center = T, scale = F),
      s_CSES = scale( CSES_t, center = T, scale = F),
      s_EIB = scale(EIB_t, center = T, scale = F)
    ) %>%
  dplyr::select(
    SNS_t, s_SES,
    s_CSES, s_EIB
  )
report::report(df_check2)

df_check2$SNS_t %>% 
  var()

df_check2$s_CSES %>% 
  var()

df_check2$s_EIB %>% 
  var()

df %>% 
  select(SNS1:SNS6) %>% 
  mutate(all = (SNS1 + SNS2 + SNS3 + SNS4 + SNS5 + SNS6)/6) %>% 
  summarise(m = mean(all),
            sd = sd(all))



valid_SNS <- "v_SNS=~ SNS1 + SNS2 + 
SNS3 + SNS4 + SNS5 + SNS6"
model_valid_SNS <- cfa(valid_SNS, estimator = "ML", likelihood = "wishart", data = df1)
fitMeasures(model_valid_SNS, 
            c("chisq", "df", "pvalue",
              "cfi", "rmsea", "tli", 'srmr'))

valid_CSES <- "v_CSES =~ CSES1 + CSES2 + 
CSES3 + CSES4"
model_valid_CSES <- cfa(valid_CSES, estimator = "GLS", data = df1)
fitMeasures(model_valid_CSES, 
            c("chisq", "df", "pvalue","cfi", 
              "rmsea", "tli", 'srmr'))
valid_EIB <- "v_EIB =~ EIB1 + EIB2 + 
EIB3 + EIB4 + EIB5 +
      EIB6 + EIB7 + EIB8"
model_valid_EIB <- cfa(valid_EIB, 
                       estimator = "GLS",data = df1)
fitMeasures(model_valid_EIB, 
            c("chisq", "df", "pvalue","cfi", 
              "rmsea", "tli", "srmr", "gfi"))














