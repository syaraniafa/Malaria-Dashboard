# app.R
library(shiny)
library(readxl)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(DT)


# -----------------------
# LOAD DATA
# -----------------------

malaria_raw <- read_excel("tahunan kasus malaria.xlsx")
malaria_data <- read_excel("Malaria2.xlsx")

required_cols <- c("Wilayah","Tahun","Kasus","Longitude","Latitude")
missing_cols <- setdiff(required_cols, names(malaria_raw))
if(length(missing_cols)>0){
  stop("Data kamu belum punya kolom: ", paste(missing_cols, collapse = ", "))
}

safe_numeric <- function(x){
  if(is.numeric(x)) return(x)
  as.numeric(as.character(x))
}

# ===========================================================
#  UI
# ===========================================================

ui <- page_sidebar(
  title = "Analisis Kasus Malaria di NTT",
  
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    )
  ),
  
  theme = bs_theme(
    bootswatch = "flatly",
    base_font  = font_google("Inter"),
    font_scale = 0.85,
    navbar_bg  = "#2C3E50"
  ),
  
  ## ============ CSS GLOBAL (NAVBAR, TABS, CARD) ================
  tags$head(
    tags$style(HTML("
      /* ================= CARD STATISTIK ================= */
      .stat-card {
        border-radius: 12px;
        padding: 14px;
        background: #ffffff;
        box-shadow: 0 3px 8px rgba(0,0,0,0.05);
      }
      
      .card-body {
        padding: 14px !important;
      }

      .stat-label {
        font-size: 14px;
        color: #6b7280;
      }

      .stat-value {
        font-size: 22px;
        font-weight: 700;
        margin-top: 4px;
      }
      
      /* ===== TABLE KORELASI ===== */
      .corr-table {
        width: 100%;
        border-collapse: collapse;
        margin-top: 10px;
      }
      
      .corr-table th {
        background: #f4f6f7;
        font-weight: 600;
        text-align: center;
        padding: 10px;
        border: 1px solid #dee2e6;
      }
      
      .corr-table td {
        text-align: center;
        padding: 10px;
        border: 1px solid #dee2e6;
      }
      
      /* Warna korelasi */
      .corr-pos-strong { background: #e8fff3; }  /* hijau */
      .corr-pos-mid    { background: #fffbd1; }  /* kuning */
      .corr-neg        { background: #ffe4e4; }  /* merah */
      
      /* Box interpretasi */
      .interpret-box {
        background: #f5f9ff;
        border-left: 5px solid #2c7be5;
        padding: 14px;
        margin-top: 15px;
        color: #2c3e50;
      }
    "))
  ),
  
  ## ============== SIDEBAR (CSS KHUSUS SIDEBAR) =================
  sidebar = tagList(
    tags$style(HTML("
      .menu-btn {
        width: 100%;
        text-align: left;
        padding: 8px 10px;
        margin-bottom: 4px;
        border: none;
        background: #f2f3f4;
        font-size: 14px;
        border-radius: 6px;
        cursor: pointer;
        color: #2C3E50;
        transition: all 0.15s ease-in-out;
      }

      .menu-btn:hover {
        background: rgba(24, 188, 156, 0.12);
        color: #18BC9C;
      }

      .menu-btn:hover i {
        color: #18BC9C;
      }

      .menu-active {
        background: #2C3E50 !important;
        color: white !important;
      }

      .menu-active i {
        color: white !important;
      }

      .sidebar-title {
        margin-top: 8px;
        margin-bottom: 12px;
        font-weight: 600;
      }
    ")),
    
    tags$div(class = "sidebar-title", "Menu"),
    actionButton("go_home", tagList(icon("house"), "Beranda"), class = "menu-btn"),
    actionButton("go_info", tagList(icon("info-circle"), "Informasi Malaria"), class = "menu-btn"),
    actionButton("go_analysis", tagList(icon("chart-bar"), "Analisis & Pemodelan Malaria"), class = "menu-btn"),
    actionButton("go_sumber", tagList(icon("book"), "Sumber Referensi"), class = "menu-btn")
  ),
  
  ## ============== MAIN CONTENT =================
  div(style = "padding: 18px;", uiOutput("main_content")),
  
  ## =============== JS ================
  tags$script(HTML("
    Shiny.addCustomMessageHandler('setActiveMenu', function(id){
      var btns = ['go_home','go_info','go_analysis','go_sumber'];
      btns.forEach(function(b){
        var el = document.getElementById(b);
        if(el) el.classList.remove('menu-active');
      });
      var sel = document.getElementById(id);
      if(sel) sel.classList.add('menu-active');
    });
  ")),
  
  tags$head(
    tags$style(HTML("
    .modal-dialog {
      display: flex;
      align-items: center;
      min-height: 100vh;
    }
  "))
  )
)


# ===========================================================
# SERVER
# ===========================================================

analysis_vars <- c(
  "JumlahKasus",
  "JumlahPendudukMiskin",
  "JumlahPuskesmas",
  "TidakadaToilet",
  "AirMinumLayak",
  "RumahLayakHuni",
  "KeluhanKesehatan",
  "JaminanKesehatan"
)

var_map <- c(
  "JumlahKasus"           = "Y",
  "JumlahPendudukMiskin"  = "X1",
  "JumlahPuskesmas"       = "X2",
  "TidakadaToilet"        = "X3",
  "AirMinumLayak"         = "X4",
  "RumahLayakHuni"        = "X5",
  "KeluhanKesehatan"      = "X6",
  "JaminanKesehatan"      = "X7"
)


var_labels <- c(
  JumlahKasus = "Jumlah Kasus Malaria",
  JumlahPendudukMiskin = "Jumlah Penduduk Miskin",
  JumlahPuskesmas = "Jumlah Puskesmas",
  TidakadaToilet = "RT Tanpa Toilet (%)",
  AirMinumLayak = "Akses Air Minum Layak (%)",
  RumahLayakHuni = "Rumah Layak Huni (%)",
  KeluhanKesehatan = "Keluhan Kesehatan (%)",
  JaminanKesehatan = "Jaminan Kesehatan (%)"
)


server <- function(input, output, session) {
  
  data_analysis <- reactive({
    malaria_data %>%
      select(any_of(analysis_vars)) %>%
      mutate(across(everything(), safe_numeric))
  })
  
  numeric_vars <- reactive({
    names(data_analysis())
  })
  
  
  statCard <- function(title, value, bg = "#F5F7FA", color = "#2C3E50") {
    div(
      style = paste0("
      background:", bg, ";
      border-radius:12px;
      padding:18px;
      min-height:110px;
      box-shadow:0 2px 6px rgba(0,0,0,0.05);
    "),
      p(title, style = "margin:0; font-size:14px; color:#6c757d;"),
      h3(
        value,
        style = paste0("
        margin-top:6px;
        font-weight:600;
        color:", color, ";
      ")
      )
    )
  }
  
  current_menu <- reactiveVal("Home")

  observeEvent(input$go_home, {current_menu("Home"); session$sendCustomMessage("setActiveMenu", "go_home")})
  observeEvent(input$go_info, {current_menu("Malaria Info"); session$sendCustomMessage("setActiveMenu", "go_info")})
  observeEvent(input$go_analysis, {current_menu("Analisis Data"); session$sendCustomMessage("setActiveMenu", "go_analysis")})
  observeEvent(input$go_sumber, {current_menu("Sumber Referensi"); session$sendCustomMessage("setActiveMenu", "go_sumber")})
  observeEvent(TRUE, {session$sendCustomMessage("setActiveMenu", "go_home")}, once = TRUE)
  observeEvent(numeric_vars(), {
    req(length(numeric_vars()) > 0)
    updateSelectInput(
      session,
      "color_by",
      choices = numeric_vars(),
      selected = "JumlahKasus"
    )
  })

  observeEvent(input$reset_wilayah, {
    updateSelectizeInput(session, "selected_wilayah", selected = character(0))
    
    showModal(
      modalDialog(
        title = "Reset Berhasil",
        "Pilihan telah direset. Silakan pilih kabupaten/kota untuk melihat tren kasus.",
        easyClose = TRUE,
        footer = modalButton("Tutup")
      )
    )
  })
  
  # ---------- MAIN CONTENT RENDERER ----------
  output$main_content <- renderUI({
    menu <- current_menu()
    
    ## =========== beranda ==========
    if (menu == "Home") {
      fluidPage(
        
        ## ===== HERO SECTION =====
        card(
          class = "border-0 text-center",
          style = "
        background: linear-gradient(135deg, #18BC9C, #2C3E50);
        color: white;
        padding: 22px 20px;
        border-radius: 12px;
      ",
          
          h2(
            "Dashboard Analisis Kasus Malaria di Nusa Tenggara Timur",
            style = "font-weight:700; margin-bottom:10px;"
          ),
          
          hr(style = "border-top: 1px solid rgba(255,255,255,0.3); width: 60%; margin: 12px auto;"),
          
          p(
            "Dashboard ini menyajikan analisis deskriptif, korelasi antar variabel, serta pemodelan statistik 
        untuk memahami faktor-faktor yang memengaruhi kasus malaria di Provinsi Nusa Tenggara Timur. 
        Hasil analisis diharapkan dapat memberikan gambaran berbasis data dalam mendukung 
        upaya pengendalian malaria di NTT.",
            style ="font-size: 1.05rem; max-width: 900px; margin: auto;"
          )
        ),
        
        br(),
        
        ## ===== FITUR UTAMA =====
        fluidRow(
          
          column(
            4,
            card(
              class = "shadow-sm text-center h-100",
              style = "border-radius:12px;",
              card_body(
                div(
                  style = "
            display:flex;
            align-items:center;
            justify-content:center;
            text-align:center;
            gap:12px;
            margin-bottom:12px;
          ",
                  div(
                    style = "
              width:45px;
              height:45px;
              background:#E8F8F5;
              border-radius:10px;
              display:flex;
              align-items:center;
              justify-content:center;
            ",
                    icon("file-alt", style = "color:#18BC9C; font-size:20px;")
                  ),
                  h5("Analisis Deskriptif", style = "margin:0;", class = "fw-bold")
                ),

                p(
                  "Menyajikan statistik ringkasan dan distribusi data untuk memberikan gambaran awal 
          karakteristik kasus malaria di NTT."
                )
              )
            )
          ),
          
          column(
            4,
            card(
              class = "shadow-sm text-center h-100",
              style = "border-radius:12px;",
              card_body(
                div(
                  style = "
            display:flex;
            align-items:center;
            justify-content:center;
            text-align:center;
            gap:12px;
            margin-bottom:12px;
          ",
                  div(
                    style = "
              width:45px;
              height:45px;
              background:#E8F8F5;
              border-radius:10px;
              display:flex;
              align-items:center;
              justify-content:center;
            ",
                    icon("project-diagram", style = "color:#18BC9C; font-size:20px;")
                  ),
                  h5("Korelasi & Multikolinearitas", style = "margin:0;", class = "fw-bold")
                ),
                
                p(
                  "Menampilkan matriks korelasi antar variabel dan membantu mengidentifikasi 
          adanya multikolinearitas sebelum pemodelan statistik."
                )
              )
            )
          ),
          
          column(
            4,
            card(
              class = "shadow-sm text-center h-100",
              style = "border-radius:12px;",
              card_body(
                div(
                  style = "
            display:flex;
            align-items:center;
            justify-content:center;
            text-align:center;
            gap:12px;
            margin-bottom:12px;
          ",
                  div(
                    style = "
              width:45px;
              height:45px;
              background:#E8F8F5;
              border-radius:10px;
              display:flex;
              align-items:center;
              justify-content:center;
            ",
                    icon("chart-line", style = "color:#18BC9C; font-size:20px;")
                  ),
                  h5("Model Poisson Ridge", style = "margin:0;", class = "fw-bold")
                ),
                
                p(
                  "Menyediakan pemodelan regresi Poisson Ridge Regression untuk data cacah yang mengandung 
          multikolinearitas agar model lebih stabil dan interpretatif."
                )
              )
            )
          )
        ),
        
        br(),
        
        ## ===== CARA PENGGUNAAN =====
        card(
          class = "shadow-sm",
          style = "border-radius:12px;",
          card_body(
            h4("Cara Penggunaan Website", class = "fw-bold"),
            tags$ol(
              tags$li(
                "Buka menu ", strong("Informasi Malaria"),
                " untuk memahami definisi malaria, gejala, cara penularan, dan pencegahannya."
              ),
              tags$li(
                "Masuk ke menu ", strong("Analisis & Pemodelan Malaria"),
                " untuk melihat statistik deskriptif, distribusi kasus, dan visualisasi tren."
              ),
              tags$li(
                "Gunakan tab ", strong("Korelasi & Multikolinearitas"),
                " untuk memahami hubungan antar variabel sebelum pemodelan."
              ),
              tags$li(
                "Lihat hasil ", strong("Model Poisson Ridge Regression"),
                " untuk mengetahui faktor-faktor yang berpengaruh terhadap jumlah kasus malaria."
              )
            )
          )
        ),
        br(),
        p("© 2025 Syarani Afa Natira Kusumah. All rights reserved.")
      )
    }
      ## =========== info malaria ==========
      else if (menu == "Malaria Info") {
      
      fluidPage(
        
        h2("Informasi Malaria"),
        br(),
        
        fluidRow(
          
          ## --- Definisi & Penyebab ---
          column(
            4,
            card(
              class = "shadow-sm h-100",
              style = "border-radius:12px",
              card_body(
                div(
                  style = "display:flex; align-items:center; gap:12px; margin-bottom:12px;",
                  div(
                    style = "
                  width:45px; height:45px; background:#FDEDEC;
                  border-radius:10px; display:flex;
                  align-items:center; justify-content:center;",
                    icon("exclamation-circle", style = "color:#E74C3C; font-size:20px;")
                  ),
                  h5("Definisi & Penyebab Malaria", style = "margin:0;", class = "fw-bold")
                ),
                p(
                  "Malaria adalah penyakit menular yang disebabkan oleh parasit ",
                  strong("Plasmodium"),
                  " yang masuk ke dalam tubuh manusia melalui gigitan nyamuk ",
                  strong("Anopheles betina"),
                  " yang terinfeksi."
                )
              )
            )
          ),
          
          ## --- Gejala & Cara Penularan ---
          column(
            4,
            card(
              class = "shadow-sm h-100",
              style = "border-radius:12px",
              card_body(
                div(
                  style = "display:flex; align-items:center; gap:12px; margin-bottom:12px;",
                  div(
                    style = "
                  width:45px; height:45px; background:#FEF5E7;
                  border-radius:10px; display:flex;
                  align-items:center; justify-content:center;",
                    icon("thermometer-half", style = "color:#F39C12; font-size:20px;")
                  ),
                  h5("Gejala & Cara Penularan", style = "margin:0;", class = "fw-bold")
                ),
                tags$ul(
                  tags$li("Demam tinggi disertai menggigil"),
                  tags$li("Sakit kepala, mual, dan nyeri otot"),
                  tags$li("Kelelahan dan berkeringat berlebihan"),
                  tags$li("Penularan terjadi melalui gigitan nyamuk Anopheles yang membawa parasit")
                )
              )
            )
          ),
          
          ## --- Pencegahan ---
          column(
            4,
            card(
              class = "shadow-sm h-100",
              style = "border-radius:12px",
              card_body(
                div(
                  style = "display:flex; align-items:center; gap:12px; margin-bottom:10px;",
                  div(
                    style = "
                  width:45px; height:45px; background:#E9F7EF;
                  border-radius:10px; display:flex;
                  align-items:center; justify-content:center;",
                    icon("shield-alt", style = "color:#27AE60; font-size:20px;")
                  ),
                  h5("Pencegahan Malaria", style = "margin:0;", class = "fw-bold")
                ),
                tags$ul(
                  tags$li("Menggunakan kelambu berinsektisida saat tidur"),
                  tags$li("Menggunakan obat atau lotion anti-nyamuk"),
                  tags$li("Menghilangkan genangan air di sekitar rumah"),
                  tags$li("Mengonsumsi obat pencegahan (profilaksis) sesuai anjuran tenaga medis")
                )
              )
            )
          )
        ),
        
        br(),
        
        ## --- Situasi Malaria di NTT ---
        card(
          class = "shadow-sm",
          style = "border-radius:12px;",
          card_body(
            h4("Situasi Malaria di Nusa Tenggara Timur", class = "fw-bold"),
            p(
              "Provinsi Nusa Tenggara Timur (NTT) merupakan salah satu daerah endemis malaria di Indonesia. ",
              "Faktor geografis, iklim tropis, serta kondisi sosial ekonomi berperan dalam tingginya risiko penularan malaria di wilayah ini."
            ),
            p(
              "Dashboard ini dirancang untuk membantu masyarakat dan peneliti memahami pola penyebaran malaria ",
              "serta mendukung pengambilan keputusan berbasis data melalui analisis statistik yang komprehensif."
            ),
            tags$ul(
              tags$li("Wilayah dengan akses kesehatan terbatas memiliki risiko lebih tinggi"),
              tags$li("Sanitasi lingkungan dan kepadatan hunian berpengaruh terhadap penularan"),
              tags$li("Upaya pencegahan dan pengendalian vektor masih perlu diperkuat")
            ),
          )
        ),

        br(),
        p("© 2025 Syarani Afa Natira Kusumah. All rights reserved.")
      )
    } ## =========== Statistik Deskriptif ==========
    else if (menu == "Analisis Data") {
      
      fluidPage(
        
        h2("Analisis Data Malaria di Provinsi Nusa Tenggara Timur"),
        
        br(),
        tabsetPanel(
          type = "pills",
          
          tabPanel(
            title = tagList(icon("file-alt"), "Analisis Deskriptif"),
            br(),
            
            card(
              class = "shadow-sm",
              style = "border-radius:12px;",
              card_body(
                
                fluidRow(
                  column(
                    4,
                    selectInput(
                      inputId = "selected_year_desc",
                      label   = "Pilih Tahun:",
                      choices = sort(unique(malaria_raw$Tahun)),
                      selected = max(malaria_raw$Tahun)
                    )
                  )
                ),
              
                ## ---------- STATISTIK DESKRIPTIF ----------
                h4("Statistik Deskriptif", class = "fw-bold"),
                
                fluidRow(
                  column(3, uiOutput("total_kasus_card")),
                  column(3, uiOutput("rata_kasus_card")),
                  column(3, uiOutput("kasus_max_card")),
                  column(3, uiOutput("kasus_min_card"))
                ),
                
                br(),
                
                ## ========== DISTRIBUSI KASUS ==========
                h4("Distribusi Kasus per Kabupaten/Kota", class = "fw-bold"),
                
                plotly::plotlyOutput("bar_kasus_kab", height = "340px"),
                uiOutput("bar_kasus_kab_interpretation"),
                
                br(),
                
                ## ========== TREN KASUS ==========
                h4("Tren Kasus Malaria per Kabupaten/Kota", class = "fw-bold"),
                
                selectizeInput(
                  "selected_wilayah",
                  "Pilih Kabupaten/Kota:",
                  choices  = sort(unique(df_trend_wilayah$Wilayah)),
                  selected = top3_2024,
                  multiple = TRUE,
                  options = list(
                    plugins = list("remove_button"),
                    placeholder = "Pilih kabupaten/kota..."
                  )
                ),
                column(
                  4,
                  actionButton("reset_wilayah", "Reset Pilihan", icon = icon("rotate"))
                ),
                
                plotlyOutput("trend_wilayah_plot", height = "380px"),
                uiOutput("trend_wilayah_interpretation"),
            )
           )
          ),
          
          ## ========= korelasi ==========
          tabPanel(
            title = tagList(icon("project-diagram"), "Korelasi & Multikolinearitas"),
            br(),
            
            card(
              class = "shadow-sm",
              style = "border-radius:12px;",
              card_body(
                h4("Analisis Korelasi", class = "fw-bold"),
                p("Matriks korelasi menunjukkan hubungan antar variabel berdasarkan data penelitian."),
                
                DT::dataTableOutput("tabel_korelasi"),
                
                # Keterangan variabel
                tags$div(
                  tags$b("Keterangan Variabel:"),
                  tags$ul(
                    tags$li("Y  : Jumlah Kasus Malaria"),
                    tags$li("X1 : Jumlah Penduduk Miskin"),
                    tags$li("X2 : Jumlah Desa/Kelurahan yang Memiliki Puskesmas"),
                    tags$li("X3 : Rumah Tangga yang Tidak Memiliki Fasilitas BAB (%)"),
                    tags$li("X4 : Rumah Tangga dengan Akses Air Minum Layak (%)"),
                    tags$li("X5 : Rumah Tangga dengan Hunian Layak (%)"),
                    tags$li("X6 : Keluhan Kesehatan Selama Sebulan Terakhir (%)"),
                    tags$li("X7 : Penduduk dengan Jaminan Kesehatan (%)")
                  )
                ),
                
                uiOutput("interpretasi_korelasi"),
              )
            ),
            
            br(),
            
            card(
              class = "shadow-sm",
              style = "border-radius:12px;",
              card_header(
                tagList(
                  icon("exclamation-triangle", class = "text-primary"),
                  span(" Apa itu Multikolinearitas?", style = "font-weight:600;")
                )
              ),
              uiOutput("multicollinearity_info")
            )
            
          ),
          
          ## ========= Model ==========
          tabPanel(
            title = "Model Poisson Ridge",
            icon = icon("chart-line"),
            
            br(),
            
            card(
              class = "shadow-sm",
              style = "border-radius:12px;",
              card_body(
                h4("Model Poisson Ridge Regression", class = "fw-bold"),
                
                p(
                  "Model Poisson Ridge Regression digunakan untuk data cacah dengan ",
                  "multikolinearitas. Parameter ridge (λ) membantu menstabilkan estimasi ",
                  "parameter regresi."
                ),
                
                h5("Koefisien Model (λ = 0.0355)", class = "mt-3"),
                DT::dataTableOutput("prr_coef_table"),
                
                tags$div(
                  tags$b("Keterangan Uji Wald: "),
                  tags$p(
                    "Pada model Poisson Ridge Regression, signifikansi parameter ",
                    "ditentukan menggunakan uji Wald berbasis distribusi Chi-Square. ",
                    "Suatu variabel dinyatakan signifikan apabila nilai statistik Wald ",
                    "lebih besar dari nilai kritis χ²(0.05;1) = 3.841."
                  ),
                ),
                
                fluidRow(
                  column(
                    width = 4,
                    statCard("MSE", "0.954", bg = "#f7f2ff", color = "#7c3aed")
                  ),
                  column(
                    width = 4,
                    statCard("RMSE", "0.977", bg = "#f1f6ff", color = "#2563eb")
                  ),
                  column(
                    width = 4,
                    statCard("Pseudo R²", "0.702", bg = "#fff1f2", color = "#e11d48")
                  )
                ),
                uiOutput("gof_interpretation"),
                uiOutput("prr_interpretation"),
              )
            )
          )
        ),
        
        br(),
        p("© 2025 Syarani Afa Natira Kusumah. All rights reserved.")
      )
    } ## ===== SUMBER DATA =====
    else if (menu == "Sumber Referensi") {
      
      fluidPage(
        
        h2("Sumber & Referensi"),
        br(),
        
        card(
          class = "shadow-sm",
          style = "border-radius:12px;",
          card_body(
            h4("Sumber Data", class = "fw-bold"),
            p(
              "Data yang digunakan dalam dashboard ini merupakan data sekunder yang bersumber 
       dari Badan Pusat Statistik (BPS) Provinsi Nusa Tenggara Timur. 
       Sumber data utama berasal dari publikasi ",
              tags$em("Provinsi Nusa Tenggara Timur Dalam Angka 2025"),
              " serta data pendukung yang diakses melalui situs resmi BPS Provinsi Nusa Tenggara Timur."
            ),
            
            tags$ul(
              tags$li(
                tags$a(
                  href = "https://ntt.bps.go.id/id/publication/2025/02/28/0486bf7b20576e8f1faa5384/provinsi-nusa-tenggara-timur-dalam-angka-2025.html",
                  "Publikasi Provinsi Nusa Tenggara Timur Dalam Angka 2025",
                  target = "_blank",
                  style = "color:#18BC9C; text-decoration:none;"
                )
              ),
              tags$li(
                tags$a(
                  href = "https://ntt.bps.go.id",
                  "Situs Resmi BPS Provinsi Nusa Tenggara Timur",
                  target = "_blank",
                  style = "color:#18BC9C; text-decoration:none;"
                )
              )
            )
          )
        ),
        
        br(),
        
        ## ===== REFERENSI INFORMASI MALARIA =====
        card(
          class = "shadow-sm",
          style = "border-radius:12px;",
          card_body(
            h4("Referensi Informasi Malaria", class = "fw-bold"),
            p(
              "Referensi berikut digunakan sebagai dasar penyusunan informasi mengenai malaria pada dashboard ini."
            ),
            tags$ul(
              tags$li(
                tags$a(
                  href = "https://malaria.kemkes.go.id/case",
                  "Kasus Malaria di Indonesia — Kementerian Kesehatan RI",
                  target = "_blank", style = "color:#18BC9C; text-decoration:none;"
                )
              ),
              tags$li(
                tags$a(
                  href = "https://ayosehat.kemkes.go.id/apa-itu-malaria",
                  "Apa Itu Malaria? — AyoSehat (Kemenkes RI)",
                  target = "_blank", style = "color:#18BC9C; text-decoration:none;"
                )
              ),
              tags$li(
                tags$a(
                  href = "https://www.cnbcindonesia.com/lifestyle/20250425110830-33-628690/kemenkes-95-kasus-malaria-di-indonesia-terjadi-di-wilayah-ini",
                  "Kemenkes: 95% Kasus Malaria di Indonesia Terjadi di Wilayah Ini — CNBC Indonesia",
                  target = "_blank", style = "color:#18BC9C; text-decoration:none;"
                )
              ),
              tags$li(
                tags$a(
                  href = "https://health.detik.com/berita-detikhealth/d-7885099/tren-malaria-di-ri-meningkat-wilayah-ini-sumbang-kasus-terbanyak",
                  "Tren Malaria di RI Meningkat, Wilayah Ini Sumbang Kasus Terbanyak — detikHealth",
                  target = "_blank", style = "color:#18BC9C; text-decoration:none;"
                )
              )
            )
          )
        ),
        
        br(),
        
        ## ===== REFERENSI METODE ANALISIS =====
        card(
          class = "shadow-sm",
          style = "border-radius:12px;",
          card_body(
            h4("Referensi Metode Analisis", class = "fw-bold"),
            p(
              "Bagian ini memuat daftar referensi berupa jurnal ilmiah, artikel, dan laporan resmi 
       yang relevan dengan topik malaria serta metode analisis statistik yang diterapkan 
       dalam penelitian ini."
            ),
            tags$ul(
              tags$li(
                HTML("Matdoan, M. Y., Otok, B. W., & Atok, R. M. (2020). 
          Modeling of quantile regression to identify factors affecting high malaria incidence in Indonesia. 
          <i>Jurnal Matematika, Statistika, dan Komputasi (JMSK)</i>, <i>16</i>(3), 417–427.")
              ),
              
              tags$li(
                HTML("Manullang, Y. D. B., Laia, N. G., Shabrina, W. K., Aprilia, S. N., & Girsang, V. I. (2025). 
          Analisis spasial kejadian malaria ditinjau dari kepadatan penduduk, akses sanitasi layak, dan 
          ketersediaan fasilitas kesehatan di kabupaten/kota Provinsi Sumatera Utara. 
          <i>SEHATI: Jurnal Kesehatan</i>, <i>5</i>(2), 188–196.")
              ),
              
              tags$li(
                HTML("Santi, V. M., Wiyono, A., & Sudarwanto. (2021). 
          Pemodelan jumlah kasus malaria di Indonesia menggunakan generalized linear model. 
          <i>Jurnal Statistika dan Aplikasinya</i>, <i>5</i>(1), 112–120.")
              ),
              
              tags$li(
                HTML("Praditia, R. K., Agustina, D., & Rini, D. S. (2020). 
          Analisis jumlah kasus malaria di wilayah Sumatera menggunakan geographically weighted 
          zero-inflated Poisson regression (GWZIPR). 
          <i>Indonesian Journal of Statistics and Its Applications</i>, <i>4</i>(4), 638–648.")
              ),
              
              tags$li(
                HTML("Sari, Y. N., Robby, R. R., Akbarita, R., & Narendra, R. (2024). 
          Perbandingan analisis regresi kuantil dan robust least trimmed square (LTS) 
          untuk mengidentifikasi faktor-faktor yang memengaruhi penyebaran penyakit malaria di Jawa Timur. 
          <i>Prosiding Seminar Nasional Universitas Insan Budi Utomo</i>, <i>5</i>(1).")
              ),
              
              tags$li(
                HTML("Aprilia, A. D., & Sofro, A. (2023). 
          Regresi zero-inflated Poisson untuk pemodelan angka positif penyakit malaria di Jawa Timur. 
          <i>MATHunesa: Jurnal Ilmiah Matematika</i>, <i>11</i>(2), 139–146.")
              ),
              
              tags$li(
                HTML("Dalimunthe, R. A., & Husein, I. (2025). 
          Zero-inflated negative binomial regression in malaria cases in North Sumatera. 
          <i>JISTech (Journal of Islamic Science and Technology)</i>, <i>10</i>(1), 107–115.")
              ),
              
              tags$li(
                HTML("Wardah, T. L., Lubis, R. S., & Aprilia, R. (2022). 
          Analisis Poisson ridge regression pada faktor yang memengaruhi kecelakaan lalu lintas di Sumatera Utara. 
          <i>FARABI: Jurnal Matematika dan Pendidikan Matematika</i>, <i>5</i>(2), 154–160.")
              ),
              
              tags$li(
                HTML("Wulandari. (2020). 
          Pemodelan Poisson ridge regression (PRR) pada banyak kematian bayi di Jawa Tengah. 
          <i>Indonesian Journal of Statistics and Its Applications</i>, <i>4</i>(2), 392–400.")
              ),
              
              tags$li(
                HTML("Putri, Y. E. (2021). 
          <i>Analisis pada kematian akibat penyakit jantung di Rumah Sakit Umum Pusat H. Adam Malik Medan 
          menggunakan Poisson ridge regression (PRR)</i> (Skripsi, Universitas Islam Negeri Sumatera Utara).")
              ),
              
              tags$li(
                HTML("Herawati, N., Sukma, H. J., Nisa, K., Nusyirwan, Saidi, S., Tiryono, & Misgiyati. (2024). 
          Poisson ridge regression for multicollinearity data: Case study of the number of maternal deaths 
          in Lampung Province, Indonesia. 
          <i>International Journal of Innovative Research in Science, Engineering and Technology (IJIRSET)</i>, <i>13</i>(1).")
              ),
              
              tags$li(
                HTML("Qasim, M., Månsson, K., Amin, M., Kibria, B. M. G., & Sjölander, P. (2020). 
          Biased adjusted Poisson ridge estimators: Method and application. 
          <i>Iranian Journal of Science and Technology, Transactions of Science</i>, <i>44</i>(6), 1775–1789.")
              )
            )
          )
        ),
        
        br(),
        
        ## ===== CATATAN =====
        div(
          style = "
            background:#E8F8F5;
            padding:15px;
            border-left:5px solid #18BC9C;
            margin-top:10px;
            color:#18BC9C;
      ",
          strong("Catatan: "),
          "Seluruh sumber dan referensi digunakan untuk keperluan edukasi dan penelitian akademik 
       dalam rangka mendukung analisis dan pengendalian malaria di Provinsi Nusa Tenggara Timur."
        ),
        
        br(),
        p("© 2025 Syarani Afa Natira Kusumah. All rights reserved.")
      )
    } else {
      tags$p("Menu tidak dikenali")
    }
  })
  
  # ---------- DESCRIPTIVE: SUMMARY, PLOT ----------
  
  df_desc_year <- reactive({
    req(input$selected_year_desc)
    
    malaria_raw %>%
      filter(Tahun == input$selected_year_desc)
  })
  
  output$total_kasus_card <- renderUI({
    statCard(
      "Total Kasus",
      format(sum(df_desc_year()$Kasus), big.mark = ",", scientific = FALSE),
      bg = "#EAFBF7",
      color = "#16A085"
    )
  })
  
  output$rata_kasus_card <- renderUI({
    statCard(
      "Rata-rata/Kabupaten",
      format(
        round(mean(df_desc_year()$Kasus)),
        big.mark = ",",
        scientific = FALSE
      ),
      bg = "#EDF4FF",
      color = "#2E6AE6"
    )
  })
  
  output$kasus_max_card <- renderUI({
    statCard(
      "Kasus Tertinggi",
      format(
        max(df_desc_year()$Kasus),
        big.mark = ",",
        scientific = FALSE
      ),
      bg = "#FFF4E5",
      color = "#E67E22"
    )
  })
  
  output$kasus_min_card <- renderUI({
    statCard(
      "Kasus Terendah",
      format(
        min(df_desc_year()$Kasus),
        big.mark = ",",
        scientific = FALSE
      ),
      bg = "#EAF7EE",
      color = "#27AE60"
    )
  })
  
  output$bar_kasus_kab <- renderPlotly({
    
    df_bar <- df_desc_year() %>%
      group_by(Wilayah) %>%
      summarise(TotalKasus = sum(Kasus, na.rm = TRUE)) %>%
      arrange(desc(TotalKasus))
    
    p <- ggplot(
      df_bar,
      aes(
        x = reorder(Wilayah, TotalKasus),
        y = TotalKasus,
        text = paste0(
          "<b>Wilayah:</b> ", Wilayah,
          "<br><b>Tahun:</b> ", input$selected_year_desc,
          "<br><b>Total Kasus:</b> ", format(TotalKasus, big.mark = ",")
        )
      )
    ) +
      geom_col(fill = "#18BC9C") +
      coord_flip() +
      labs(
        x = "Kabupaten/Kota",
        y = "Total Kasus"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$bar_kasus_kab_interpretation <- renderUI({
    
    df_bar <- df_desc_year() %>%
      group_by(Wilayah) %>%
      summarise(TotalKasus = sum(Kasus, na.rm = TRUE)) %>%
      arrange(desc(TotalKasus))
    
    wilayah_max <- df_bar$Wilayah[1]
    max_val     <- df_bar$TotalKasus[1]
    
    wilayah_min <- df_bar$Wilayah[nrow(df_bar)]
    min_val     <- df_bar$TotalKasus[nrow(df_bar)]
    
    # Wilayah dengan kasus 0
    wilayah_zero <- df_bar %>%
      filter(TotalKasus == 0) %>%
      pull(Wilayah)
    
    tagList(
      
      # Interpretasi utama
      p(
        HTML(paste0(
          "Pada tahun <b>", input$selected_year_desc, "</b>, ",
          "<b>", wilayah_max, "</b> merupakan wilayah dengan jumlah kasus tertinggi (",
          format(max_val, big.mark=","), " kasus), ",
          "sehingga dapat diprioritaskan dalam upaya pengendalian malaria."
        ))
      ),
      
      # Wilayah terendah
      p(
        HTML(paste0(
          "Wilayah dengan jumlah kasus terendah adalah <b>", wilayah_min,
          "</b> dengan ", format(min_val, big.mark=","), " kasus."
        ))
      ),
      
      br(),
      p(
        style = "font-style: italic; color: #6c757d;",
        "Arahkan kursor ke bar pada grafik untuk melihat jumlah kasus secara rinci."
      )
    )
  })
  
  # ---------- Trend Plot ----------
  
  df_trend_wilayah <- malaria_raw %>%
    mutate(
      Tahun = as.numeric(Tahun),
      Kasus = as.numeric(Kasus)
    ) %>%
    group_by(Wilayah, Tahun) %>%
    summarise(
      TotalKasus = sum(Kasus, na.rm = TRUE),
      .groups = "drop"
    )
  
  top3_2024 <- df_trend_wilayah %>%
    filter(Tahun == 2024) %>%
    arrange(desc(TotalKasus)) %>%
    slice_head(n = 3) %>%
    pull(Wilayah)
  
  output$trend_wilayah_plot <- renderPlotly({
    
    req(input$selected_wilayah)
    
    df_plot <- df_trend_wilayah %>%
      filter(Wilayah %in% input$selected_wilayah)
    
    p <- ggplot(
      df_plot,
      aes(
        x = Tahun,
        y = TotalKasus,
        color = Wilayah,
        group = Wilayah,
        text = paste0(
          "<b>Wilayah:</b> ", Wilayah,
          "<br><b>Tahun:</b> ", Tahun,
          "<br><b>Total Kasus:</b> ", format(TotalKasus, big.mark = ",")
        )
      )
    ) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      labs(
        x = "Tahun",
        y = "Total Kasus",
        color = "Wilayah"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$trend_wilayah_interpretation <- renderUI({
    
    req(input$selected_wilayah)
    
    df_selected <- df_trend_wilayah %>%
      filter(Wilayah %in% input$selected_wilayah) %>%
      arrange(Wilayah, Tahun)
    
    interpret_list <- lapply(unique(df_selected$Wilayah), function(w) {
      
      df_w <- df_selected %>% filter(Wilayah == w)
      
      tahun_awal <- min(df_w$Tahun)
      tahun_akhir <- max(df_w$Tahun)
      
      kasus_awal <- df_w$TotalKasus[df_w$Tahun == tahun_awal]
      kasus_akhir <- df_w$TotalKasus[df_w$Tahun == tahun_akhir]
      
      perubahan <- kasus_akhir - kasus_awal
      persen_perubahan <- round((perubahan / kasus_awal) * 100, 1)
      
      # Kategori tren
      tren <- if (perubahan > 0) {
        "<b>meningkat</b>"
      } else if (perubahan < 0) {
        "<b>menurun</b>"
      } else {
        "<b>relatif stabil</b>"
      }
      
      tags$div(
        HTML(paste0(
          "<b>", w, "</b> menunjukkan tren kasus malaria yang ", tren,
          " dari tahun ", tahun_awal, " hingga ", tahun_akhir, ". ",
          "Jumlah kasus berubah dari <b>", format(kasus_awal, big.mark=","), 
          "</b> menjadi <b>", format(kasus_akhir, big.mark=","), 
          "</b> (", persen_perubahan, "%)."
        ))
      )
      
    })
    
    tagList(
      interpret_list,
      p(
        style = "font-style: italic; color: #6c757d; margin-top:10px;",
        "Arahkan kursor ke titik pada grafik untuk melihat jumlah kasus detail per tahun."
      )
    )
  })
  
  # ---------- CORRELATION ----------
  
  output$tabel_korelasi <- DT::renderDataTable({
    
    analysis_vars <- c(
      "JumlahKasus",
      "JumlahPendudukMiskin",
      "JumlahPuskesmas",
      "TidakadaToilet",
      "AirMinumLayak",
      "RumahLayakHuni",
      "KeluhanKesehatan",
      "JaminanKesehatan"
    )
    
    var_map <- c(
      "JumlahKasus"           = "Y",
      "JumlahPendudukMiskin"  = "X1",
      "JumlahPuskesmas"       = "X2",
      "TidakadaToilet"        = "X3",
      "AirMinumLayak"         = "X4",
      "RumahLayakHuni"        = "X5",
      "KeluhanKesehatan"      = "X6",
      "JaminanKesehatan"      = "X7"
    )
    
    corr_mat <- malaria_data |>
      dplyr::select(all_of(analysis_vars)) |>
      cor(use = "pairwise.complete.obs")
    
    colnames(corr_mat) <- var_map[colnames(corr_mat)]
    rownames(corr_mat) <- var_map[rownames(corr_mat)]
    
    corr_df <- as.data.frame(round(corr_mat, 2))
    corr_df$Variabel <- rownames(corr_df)
    
    corr_df <- corr_df |> dplyr::select(Variabel, everything())
    
    DT::datatable(
      corr_df,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE
      )
    ) |>
      
      DT::formatStyle(
        columns = unname(var_map),
        backgroundColor = DT::styleInterval(
          c(-0.6, -0.3, 0.3, 0.6),
          c(
            "#f5b7b1",  # negatif kuat
            "#fadbd8",  # negatif sedang
            "white",    # lemah
            "#eafaf1",  # positif sedang
            "#7dcea0"   # positif kuat
          )
        )
      )
    
  })
  
  output$interpretasi_korelasi <- renderUI({
    tags$div(
      class = "p-3",
      style = "
      background:#eaf4ff;
      border-left:4px solid #2c7be5;
      color:#2c7be5;
    ",
      
      tags$b("Interpretasi Korelasi:"), tags$br(), tags$br(),
      
      "Korelasi positif kuat ditemukan antara X4 (Air Minum Layak) dan X5 (Rumah Layak Huni) 
    dengan nilai sebesar 0.71. Hal ini menunjukkan bahwa wilayah dengan akses air minum 
    yang lebih baik cenderung memiliki kondisi hunian yang lebih layak. Kedua variabel ini 
    secara konseptual merepresentasikan tingkat kesejahteraan dan kualitas lingkungan tempat tinggal.", 
      
      tags$br(), tags$br(),
      
      "Korelasi negatif kuat terjadi antara X3 (Rumah Tangga Tidak Memiliki Toilet) dan 
    X5 (Rumah Layak Huni) sebesar -0.67, serta antara X3 dan X4 sebesar -0.52. 
    Hal ini menunjukkan bahwa semakin tinggi persentase rumah tangga tanpa fasilitas 
    sanitasi, maka kualitas hunian dan akses air minum layak cenderung lebih rendah. 
    Hubungan ini konsisten dengan kondisi sosial ekonomi wilayah.", 
      
      tags$br(), tags$br(),
      
      "Hubungan antara Y (Jumlah Kasus Malaria) dan X3 sebesar 0.60 menunjukkan 
    korelasi positif yang cukup kuat. Artinya, wilayah dengan tingkat sanitasi 
    yang buruk cenderung memiliki jumlah kasus malaria yang lebih tinggi. 
    Secara epidemiologis, kondisi sanitasi yang tidak memadai dapat meningkatkan 
    risiko berkembangnya lingkungan yang mendukung penyebaran penyakit berbasis vektor."
    )
  })
  
  output$multicollinearity_info <- renderUI({
    HTML("
  <!-- DEFINISI -->
  <div>
    <b>Multikolinearitas</b> adalah kondisi ketika dua atau lebih variabel prediktor
    dalam suatu model regresi memiliki hubungan linier yang kuat satu sama lain.
  </div>

  <!-- PENJELASAN KORELASI -->
  <p style='margin-bottom:10px;'>
    Dalam konteks <b>matriks korelasi</b>, multikolinearitas biasanya ditandai oleh
    nilai korelasi absolut antar variabel prediktor yang relatif tinggi,
    umumnya melebihi
    <span style='
      background:#FADBD8;
      padding:2px 6px;
      border-radius:4px;
      font-weight:600;
    '>0.6</span>
    atau
    <span style='
      background:#FADBD8;
      padding:2px 6px;
      border-radius:4px;
      font-weight:600;
    '>0.7</span>.
  </p>

  <hr style='margin:12px 0;' />

  <!-- DAMPAK -->
  <p><b>Mengapa multikolinearitas perlu diperhatikan?</b></p>
  <ul style='padding-left:18px;'>
    <li>Koefisien regresi menjadi <i>tidak stabil</i> dan sensitif terhadap perubahan data.</li>
    <li>Sulit menginterpretasikan pengaruh masing-masing variabel secara terpisah.</li>
    <li>Standar error meningkat sehingga uji signifikansi menjadi kurang reliabel.</li>
  </ul>

  <!-- IMPLIKASI -->
  <div style='
    background:#FFF4E5;
    padding:14px;
    border-left:4px solid #E67E22;
    margin-bottom:14px;
    color: #E67E22
  '>
    <b>Implikasi pada analisis ini</b><br/>
    Adanya multikolinearitas antar variabel lingkungan seperti sanitasi,
    akses air minum, dan kualitas hunian menunjukkan bahwa variabel-variabel
    tersebut saling berkaitan dalam merepresentasikan kondisi wilayah.
    Oleh karena itu, digunakan pendekatan pemodelan yang lebih robust terhadap
    multikolinearitas, yaitu <b>regresi ridge</b>, untuk menghasilkan estimasi
    parameter yang lebih stabil.
  </div>
  ")
  })
  
  
  # ---------- MODEL ----------
  
  model_prr <- reactive({
    
    df <- malaria_data %>%
      select(
        JumlahKasus,
        JumlahPendudukMiskin,
        JumlahPuskesmas,
        TidakadaToilet,
        AirMinumLayak,
        RumahLayakHuni,
        KeluhanKesehatan,
        JaminanKesehatan
      ) %>%
      mutate(across(everything(), safe_numeric)) %>%
      drop_na()
    
    y <- df$JumlahKasus
    X <- as.matrix(df %>% select(-JumlahKasus))
    X_int <- cbind(1, X)
    colnames(X_int)[1] <- "(Intercept)"
    
    glm_fit <- glm(JumlahKasus ~ ., family = poisson, data = df)
    beta_glm <- coef(glm_fit)
    mu_hat <- fitted(glm_fit)
    A <- diag(as.vector(mu_hat))
    
    k_estimate <- 0.0355
    I <- diag(ncol(X_int))
    
    beta_prr <- solve(k_estimate * I + t(X_int) %*% A %*% X_int) %*%
      (t(X_int) %*% A %*% X_int) %*%
      beta_glm
    
    # SIMPAN DALAM DATA FRAME
    data.frame(
      Variabel = colnames(X_int),
      Koefisien = as.numeric(beta_prr),
      row.names = NULL
    )
  })
  
  
  prr_results <- reactive({
    
    prr_df <- model_prr() %>%
      filter(Variabel != "(Intercept)")
    
    # Wald statistic
    wald_values <- c(
      0.0001,
      15.285,
      65.496,
      1.598,
      5.780,
      25.230,
      0.511
    )
    
    chi_sq_crit <- qchisq(0.95, df = 1)  # = 3.841
    
    prr_df %>%
      mutate(
        Wald = wald_values,
        Keputusan = ifelse(Wald > chi_sq_crit,
                           "Signifikan",
                           "Tidak Signifikan")
      )
  })

  output$prr_coef_table <- DT::renderDataTable({
    
    df <- prr_results() %>%
      transmute(
        Variabel,
        Koefisien,
        Wald,
        Keputusan
      )
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = nrow(df),
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      )
    ) %>%
      DT::formatStyle(
        "Keputusan",
        backgroundColor = DT::styleEqual(
          c("Signifikan", "Tidak Signifikan"),
          c("#E3FCEF", "")
        ),
        fontWeight = DT::styleEqual(
          c("Signifikan", "Tidak Signifikan"),
          c("bold", "normal")
        )
      )
  })
  
  output$gof_interpretation <- renderUI({
    
    div(
      HTML(
        "<b>Interpretasi Goodness of Fit:</b><br>
       Nilai Pseudo R² sebesar <b>0.702</b> menunjukkan bahwa model memiliki 
       kemampuan penjelasan yang <b>kuat</b> dalam menjelaskan variasi 
       jumlah kasus malaria di Provinsi Nusa Tenggara Timur. Hal ini mengindikasikan 
        bahwa variabel-variabel prediktor yang digunakan dalam model telah mampu menjelaskan 
        sebagian besar variasi jumlah kasus malaria, sehingga model dinilai cukup baik untuk 
        keperluan analisis dan interpretasi kebijakan."
      )
    )
  })
  
  output$prr_interpretation <- renderUI({
    
    df <- model_prr() %>%
      filter(Variabel != "(Intercept)")
    
    # Ambil koefisien sesuai variabel skripsi
    beta_x2 <- df$Koefisien[df$Variabel == "JumlahPuskesmas"]
    beta_x3 <- df$Koefisien[df$Variabel == "TidakadaToilet"]
    beta_x5 <- df$Koefisien[df$Variabel == "RumahLayakHuni"]
    beta_x6 <- df$Koefisien[df$Variabel == "KeluhanKesehatan"]
    
    # Hitung IRR
    irr_x2 <- round(exp(beta_x2), 4)
    irr_x3 <- round(exp(beta_x3), 4)
    irr_x5 <- round(exp(beta_x5), 4)
    irr_x6 <- round(exp(beta_x6), 4)
    
    div(
      style = "
      background:#ecfdf5;
      border-left:6px solid #10b981;
      padding:18px 22px;
      margin-top:20px;
    ",
      
      h5("Interpretasi Model", style="font-weight:600; margin-bottom:12px;"),
      
      tags$p(
        HTML(paste0(
          "<b>Jumlah Puskesmas (X2)</b> memiliki koefisien sebesar <b>",
          round(beta_x2,4),
          "</b>. Setiap peningkatan satu satuan jumlah puskesmas akan meningkatkan jumlah kasus malaria sebesar <b>",
          irr_x2,
          " kali</b> dengan asumsi variabel lain tetap. Kondisi ini dapat terjadi karena semakin banyak fasilitas kesehatan, semakin tinggi kemampuan deteksi dan pelaporan kasus."
        ))
      ),
      
      tags$p(
        HTML(paste0(
          "<b>Tidak Ada Toilet (X3)</b> memiliki koefisien sebesar <b>",
          round(beta_x3,4),
          "</b>. Setiap peningkatan satu persen rumah tangga tanpa fasilitas tempat pembuangan air besar akan meningkatkan jumlah kasus malaria sebesar <b>",
          irr_x3,
          " kali</b> dengan asumsi variabel lain tetap. Hal ini menunjukkan bahwa sanitasi yang buruk dapat meningkatkan risiko penularan penyakit berbasis lingkungan."
        ))
      ),
      
      tags$p(
        HTML(paste0(
          "<b>Rumah Layak Huni (X5)</b> memiliki koefisien sebesar <b>",
          round(beta_x5,4),
          "</b>. Setiap peningkatan satu persen rumah tangga dengan hunian layak akan meningkatkan jumlah kasus malaria sebesar <b>",
          irr_x5,
          " kali</b> dengan asumsi variabel lain tetap. Hal ini dapat mengindikasikan adanya faktor lain seperti kepadatan penduduk atau perbedaan karakteristik wilayah."
        ))
      ),
      
      tags$p(
        HTML(paste0(
          "<b>Keluhan Kesehatan (X6)</b> memiliki koefisien sebesar <b>",
          round(beta_x6,4),
          "</b>. Setiap peningkatan satu persen keluhan kesehatan akan meningkatkan jumlah kasus malaria sebesar <b>",
          irr_x6,
          " kali</b> dengan asumsi variabel lain tetap. Semakin tinggi keluhan kesehatan, semakin besar kemungkinan kasus malaria terdeteksi."
        ))
      ),
      
      div(
        h5("Implikasi Kebijakan", style="font-weight:600; margin-bottom:12px;"),
        HTML(paste0(
        "Hasil model menunjukkan bahwa faktor <b>sanitasi</b> dan <b>akses kesehatan</b> 
   merupakan determinan penting. Oleh karena itu, intervensi berbasis 
   peningkatan sanitasi lingkungan dan penguatan layanan kesehatan 
   dapat menjadi strategi prioritas dalam pengendalian malaria di NTT."
        ))
      )
    )
  })
  
}

shinyApp(ui, server)
