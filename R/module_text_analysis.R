source("R/module_set_proj.R")

# load --------------------------------------------------------------------
tidy_twdata <- read_csv("input/tidy_twdata.csv")

plot_dates <- as_date(min(as_date(tidy_twdata$created_at)):max(as_date(tidy_twdata$created_at)))
                      

# show tweet timeline -----------------------------------------------------
png_background <- readPNG("input/twitter_background_transparent.png")

gg_lengths_and_dates <- ggplot(data = tidy_twdata, aes(x = created_at, y = display_text_width)) +
     #background_image(png_background) + 
     # annotation_raster(raster = png_background, xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf) +
     annotation_custom(grob = rasterGrob(png_background), 
                       -Inf, Inf, 
                       60, 280) +
     stat_smooth(size = 1.5, colour = "white", se = FALSE, method = "loess", n = nrow(tidy_twdata)) +
     geom_point(alpha = 0.8, size = 4, shape = "circle") +
     scale_y_continuous(minor_breaks = NULL) +
     scale_x_datetime(date_labels = "%d %b %y", date_breaks = "weeks") +
     labs(title = "#100DaysOfWriting",
          subtitle = "Lengths of my posts during the challange",
          x = "Date Created", y = "Display Text Width") +
        theme_100daysofwriting()

ggsave(filename = "output/gg_lengths_and_dates.png", 
       plot = gg_lengths_and_dates,
       width = 7, height = 4, 
       units = "in",
       dpi = 300)

# prepare tokens ----------------------------------------------------------
# create dictionary of English words
english_words_dictionary <- qdapDictionaries::GradyAugmented

# create token cleaner function
clean_tokens <- function(raw_token, empty_as_na = FALSE){
        raw_token <- gsub("(\\b100daysofwriting\\b)", x = raw_token, ignore.case = TRUE, replacement = "") # remove specific hashtag
        raw_token <- gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", x = raw_token, replacement = "") # remove urls
        raw_token <- gsub("http\\S+\\s*", x = raw_token, replacement = "") # remove https
        raw_token <- gsub("[^[:alnum:][:blank:]?&/\\-]", x = raw_token, replacement = "") # remove non printable or special characters such as emoji
        raw_token <- gsub("U00..", x = raw_token, replacement = "") # remove leftover of non printable or special characters such as emoji
        raw_token <- gsub("\\d+",x = raw_token, replacement = "") # remove all digits
        raw_token <- gsub("\\S+\\.co+\\b", x = raw_token, replacement = "") # removes .com and .co
        raw_token <- gsub("[[:punct:]]", x = raw_token, replacement = "") # remove punctuations
        raw_token <- trimws(x = raw_token, which = "both") # remove trailing
        raw_token <- gsub("[[:space:]]+", x = raw_token, replacement = " ") # remove double space
        # remove specific words
        raw_token <- gsub("(\\bday\\b)|(\\bDay\\b)", x = raw_token, replacement = "") # remove "day"
        if (empty_as_na == TRUE) {
                raw_token <- gsub("^$", replacement = NA, x = trimws(raw_token))
                tidy_token <- raw_token
                return(tidy_token)
                }
        tidy_token <- raw_token
        return(raw_token)
}

# create capitalise to sentence function
capitalise <- function(x){
        ifelse(!is.na(x), paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)), "."), NA)
        }


# create word tokens
token_words <- tidy_twdata %>%
        unnest_tokens(word, text, token = "words") %>%
        anti_join(stop_words) %>%
        filter(word %in% english_words_dictionary) %>%
        mutate(# remove "day"
                word = gsub("(\\bday\\b)|(\\bDay\\b)", x = word, replacement = ""),
                # replace empty as NA
                word = gsub("^$", replacement = NA, x = trimws(word))) %>%
        drop_na(word)

# create sentence tokens
token_sentences <- tidy_twdata %>%
        unnest_tokens(sentences, text, token = "sentences") %>%
        mutate(sentences = clean_tokens(sentences, empty_as_na = TRUE),
               sentences = capitalise(sentences)) %>%
        drop_na(sentences)


# aggregate tokens ----------------------------------------------------------
freq_token_words <- token_words %>%
        count(word, sort = TRUE) %>%
        mutate(total = sum(n),
               word = reorder(word, n),
               proportion = n / sum(n))

date_ym_fct <- c("2020 July", "2020 August", "2020 September", 
                 "2020 October", "2020 November", "2020 December",
                 "2021 February", "2021 January")

freq_rank_token_words <- freq_token_words %>% 
        mutate(rank = row_number(), 
               word_frequency = n/total)

# sentence sample 

set.seed(2021)
sample_token_sentences <- token_sentences %>%
        mutate(bin = cut(display_text_width, 20)) %>%
        group_by(bin) %>%
        slice_sample(n = 1) %>%
        ungroup() %>%
        arrange(!display_text_width) %>%
        mutate(row = row_number())

# tf-idf
tfidf_token_words_month <- token_words %>%
        mutate(month_date = paste0(year(created_at), " ", months.Date(created_at)),
               month_date = factor(month_date, levels = date_ym_fct, date_ym_fct, ordered = TRUE)) %>% 
        count(word, month_date, sort = TRUE) %>%
        mutate(total = sum(n),
               word = reorder(word, n),
               proportion = n / sum(n)) %>%
        bind_tf_idf(word, month_date, n) %>%
        arrange(desc(tf_idf))

# sentiment 
bing <- get_sentiments("bing")

sentiment_token_overall <-  token_words %>%
        inner_join(bing) %>%
        count(word, sentiment, sort = TRUE) %>%
        mutate(rownumber = row_number(),
               total = sum(n),
               word = reorder(word, n),
               proportion = n / sum(n))

sentiment_average_month <-  token_words %>%
        mutate(month_date = paste0(year(created_at), " ", months.Date(created_at)),
               month_date = factor(month_date, levels = date_ym_fct, date_ym_fct, ordered = TRUE)) %>%
        inner_join(bing) %>%
        count(word, sentiment, month_date, sort = TRUE) %>%
        pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
        mutate(sentiment = positive - negative) %>%
        group_by(month_date) %>%
        summarise(average_sentiment = mean(sentiment, na.rm = TRUE),
                  sd_sentiment = sd(sentiment, na.rm = TRUE))

sentiment_token_words_month <- token_words %>%
        mutate(month_date = paste0(year(created_at), " ", months.Date(created_at)),
               month_date = factor(month_date, levels = date_ym_fct, date_ym_fct, ordered = TRUE)) %>%
        inner_join(bing) %>%
        count(word, sentiment, month_date, sort = TRUE) %>% 
        group_by(month_date) %>% 
        mutate(n_total_month = sum(n))

# viz - distributions -----------------------------------------------------
# distribution 1 - histogram
ggplot(freq_token_words, aes(x = n)) +
        geom_histogram(bins = 50) +
        # play with the lims
        facet_zoom(ylim = c(0, 100), xlim = c(20, 40)) +
        labs_100daysofwriting(x = "Frequency [word]", y = "Frequency [occurance]",
                              title = "Word Occurance/Frequency", caption_width = 135,
                              caption = "The histogram shows how many times a word occurred (x-axis) and how prevalent was this occurance in the sample (y-axis).
                              For example, the histogram shows a long right tail (right panel), i.e., over 400 words occured only once. Zoomed histogram (left panel) illustrates that words occuring 20 or more times were rare. Note that word tokens 'day'/'days' were removed from the corpus.")  +
        theme_100daysofwriting() +
        theme(strip.background = element_rect(fill = "grey20", colour = NA)) +
        geom_curve(aes(x = 4, y = 400, xend = 40, yend = 10),
                   size = 1, color = "white",
                   arrow = arrow(length = unit(0.045, "npc"))) + 
        geom_text(aes(x = 40, y = 35), 
                  colour = "white", family = "Inconsolata",
                  label = "Rare\nwords") + 
        geom_text(aes(x = 10, y = 400), 
                  colour = "white", family = "Inconsolata",
                  label = "Frequent\nwords")

# distribution 2 - Zipf's law
ggplot(freq_rank_token_words, aes(x = rank, y = word_frequency)) + 
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
        geom_abline(intercept = -1.3370, slope = -0.6825,
                    color = "#CC3333", linetype = 2) +
        scale_x_log10() +
        scale_y_log10() +
        labs_100daysofwriting(x = "rank of word [log]", y = "frequnecy of word [log]", 
                              title = "Frequency of words by rank",
                              caption = "The graph illustrates what is known as Zipf's law.
                              This can be also observed in the histograms.
                              The law states that there will be small number of words occuring very frequently and large number of words occuring very rarely (i.e., usually once)") +
        theme_100daysofwriting() + 
        geom_text(aes(x = 1.5, y = 0.09), 
                  colour = "white", family = "Inconsolata",
                  label = "Lots of words\noccuring rarely") + 
        geom_text(aes(x = 400, y = 0.002), 
                  colour = "white", family = "Inconsolata",
                  label = "Few words\noccuring frequently")


# viz - frequencies -------------------------------------------------------
# Frequency of words 1
ggplot(freq_token_words %>% slice_max(word, n = 15), aes(n, word)) +
        geom_col() +
        labs_100daysofwriting(x = NULL, y = NULL, 
                              title = "Frequency of words used during the challange",
                              caption = "Top 15 words are selected. Note that word tokens 'day'/'days' were removed from the corpus.") +
        theme_100daysofwriting()

# Relative frequency of words 
ggplot(freq_token_words %>% slice_max(word, n = 15), aes(proportion, word)) +
        geom_col() +
        scale_x_continuous(labels = scales::percent_format()) +
        labs_100daysofwriting(title = "Relative frequency of words used during the challange",
                caption = "Top 15 words are selected. Note that word tokens 'day'/'days' were removed from the corpus.") +
        theme_100daysofwriting()  

# Sample of sentences
ggplot(sample_token_sentences %>%
               mutate(sentences = wrap_caption(sentences, max_width = 50)),
       aes(y = display_text_width, x = row)) +
        expand_limits(y = c(-50, 220), x = c(0, 17.5)) +
        geom_text_repel(aes(label = sentences, angle = -50),
                        color = "#CC3333",
                        segment.color = "gray50",
                        direction = "y",
                        family = "Inconsolata",
                        seed = 120,
                        ylim = c(-45, 200),
                        max.overlaps = Inf,
                        max.time = 20,
                        force = 15,
                        force_pull = 0) +
        geom_point(color = "gray50", size = 2, alpha = 0.5) +
        scale_y_continuous() +
        labs_100daysofwriting(title = "sample of sentences", y = "text width",
                              caption = "The random sample consists of 16 sentences. First, the text width was split into bins and one sentence was selected randomly from each bin.") +
        theme_100daysofwriting() +
        theme(axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank())

# Sample sentences table
table_sentences <- sample_token_sentences %>%
        mutate(sentences = wrap_caption(sentences, max_width = 150),
               created_at = as_date(created_at)) %>%
        select("Display Width" = display_text_width, "Sentences" = sentences, "Date" = created_at) %>%
        flextable() %>%
        autofit() %>%
        theme_zebra() %>%
        font(part = "body", j = 1:3, fontname = "Inconsolata") %>%
        font(part = "header", j = 1:3, fontname = "Proza") %>%
        align(align = "left", part = "all" ) %>%
        set_caption(caption = "Random Sample of Sentences Stratified by Display Width")

table_sentences


# viz - tf-idf ------------------------------------------------------------
custom_palette <- colorRampPalette(colors = c("#e59999", "#CC3333"))

# uses the same data but takes only frequency (relative)
tfidf_token_words_month %>% 
        group_by(month_date) %>%
        slice_max(proportion, n = 10) %>% 
        ungroup() %>%
        ggplot(aes(y = reorder_within(word, proportion, month_date), x = proportion, fill = month_date)) +
        geom_col(show.legend = FALSE) +
        scale_y_reordered() +
        scale_x_continuous(labels =  scales::percent_format()) +
        scale_fill_manual(values = custom_palette(8)) +
        facet_wrap(~ month_date, nrow = 2, scales = "free") +
        labs_100daysofwriting(
                title = "Relative frequency of the most common words",
                caption = "The words shown here are approximately top 10 words per each month. The x-axis shows relative frequency of those words in the corpus."
                ) +
        theme_100daysofwriting(default_size = 40) 

# uses the same data but does the tf-idf
tfidf_token_words_month %>% 
        group_by(month_date) %>%
        slice_max(tf_idf, n = 7) %>%
        ungroup() %>%
        ggplot(aes(y = reorder_within(word, tf_idf, month_date), x = tf_idf, fill = month_date)) +
        geom_col(show.legend = FALSE) +
        scale_y_reordered() +
        scale_fill_manual(values = custom_palette(8)) +
        facet_wrap(~ month_date, nrow = 2, scales = "free") +
        labs_100daysofwriting(title = "TF-IDF",
                              caption = "The words shown here are approximately top 10 words per each month. TF-IDF attempts to shows words that are distinctive of each month.") +
        theme_100daysofwriting(default_size = 40) 

# viz - sentiment ---------------------------------------------------------
custom_palette_sentiment <- colorRampPalette(colors = c("#CC3333", "#33cc80"))

sentiment_token_overall %>%
        slice_max(n, n = 15) %>%
        ggplot(aes(y = reorder_within(word, n, sentiment), x = n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        scale_y_reordered() + 
        scale_fill_manual(values = custom_palette_sentiment(2)) +
        facet_wrap( ~ sentiment, scales = "free_y") +
        labs_100daysofwriting(title = "sentiment of the most freqent words",
                              caption = "The words shown here are approximately top 10 words.") +
        theme_100daysofwriting(default_size = 40) 

sentiment_average_month %>%
        ggplot(aes(x = month_date, y = average_sentiment, group = 1, colour = average_sentiment)) +
        geom_line(size = 1.5) +
        geom_point(size = 4, alpha = 2) +
        geom_errorbar(aes(ymin = average_sentiment - sd_sentiment, ymax = average_sentiment + sd_sentiment),
                      size = 1) +
        scale_colour_gradient(low = "#CC3333", high = "#33cc80", guide = FALSE) +
        labs_100daysofwriting(title = "Sentiment during the challange",
                              caption = "The graph shows aggregate computed as difference between number of positive and number of negative words. The difference is then averaged over the months. Standard deviation is plotted as error bars around the mean.") +
        theme_100daysofwriting(default_size = 40) 


sentiment_token_words_month %>%
        filter(n > 1) %>%
        ggplot(aes(y = n_total_month, x = month_date, group = 1)) +
        geom_line(color = "white", lwd = .8) +
        geom_point(size = 5) +
        geom_text_repel(aes(label = word, colour = sentiment),
                        segment.color = "white",
                        direction = "both",
                        family = "Inconsolata",
                        segment.curvature = -0.1,
                        segment.ncp = 10,
                        segment.angle = 15,
                        seed = 110,
                        max.overlaps = Inf,
                        max.time = 20,
                        force = 5,
                        force_pull = 0) +
        scale_colour_manual(values = custom_palette_sentiment(2)) +
        labs_100daysofwriting(y = "total number of words\nwith sentiment") +
        theme_100daysofwriting(default_size = 40) +
        guides(color = "none")
