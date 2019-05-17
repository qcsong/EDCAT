participantCode <-
"pt1"
resultsSoFar <-
structure(list(`1` = structure(list(data = structure(list(`_id` = structure(list(
    `$type` = c("id", "id", "id", "id"), id = c("5cd2fe5783071a044106b238",
    "5cd2fe5783071a11eb04f63a", "5cd2fe5783071a15640377f8", "5cd2fe5783071a1abb020f19"
    )), .Names = c("$type", "id"), class = "data.frame", row.names = c(NA,
4L)), event_type = c("response", "response", "response", "response"
), question_code = c("ignored1", "mc:2", "mc:3", "mc:4"), question_type = c("information",
"instruction", "q_yesno", "q_select_multiple"), response = c("link clicked: false",
NA, "No!", NA), response_value = c(NA, "20", "30", "40"), session = c("947ACA2D-D9A6-44AE-96CC-99B8A4271BDE",
"947ACA2D-D9A6-44AE-96CC-99B8A4271BDE", "947ACA2D-D9A6-44AE-96CC-99B8A4271BDE",
"947ACA2D-D9A6-44AE-96CC-99B8A4271BDE"), more_data = structure(list(
    random_card = structure(list(bin_name = c(NA, "bin-1", NA,
    NA)), .Names = "bin_name", class = "data.frame", row.names = c(NA,
    4L)), options = list(NULL, NULL, structure(list(name = c("Yes",
    "No"), selected = c(FALSE, TRUE), value = c("Yes", "No")), .Names = c("name",
    "selected", "value"), class = "data.frame", row.names = 1:2),
        structure(list(name = c("helo", "bye", "aloha", "D"),
            selected = c(FALSE, TRUE, FALSE, TRUE), value = c("1",
            "2", "3", "4")), .Names = c("name", "selected", "value"
        ), class = "data.frame", row.names = c(NA, 4L)))), .Names = c("random_card",
"options"), class = "data.frame", row.names = c(NA, 4L)), response_values = list(
    NULL, NULL, NULL, c("2", "4")), responses = list(NULL, NULL,
    NULL, c("bye", "D"))), .Names = c("_id", "event_type", "question_code",
"question_type", "response", "response_value", "session", "more_data",
"response_values", "responses"), class = "data.frame", row.names = c(NA,
4L)), metadata = structure(list(id = c("58DF925D-CCFB-4C9B-9835-5CEFC7C4B16A",
"265806C6-3E2F-4A42-9366-851188FF927D", "606778C3-6202-4058-B27E-45EFCFE3C08E",
"E87BD51D-B6F1-4AED-9CC9-711DF9066CD3"), pt = c("mark1", "mark1",
"mark1", "mark1"), timestamp = c("2019-05-08T11:05:43-05:00",
"2019-05-08T11:05:43-05:00", "2019-05-08T11:05:43-05:00", "2019-05-08T11:05:43-05:00"
)), .Names = c("id", "pt", "timestamp"), class = "data.frame", row.names = c(NA,
4L))), .Names = c("data", "metadata"), class = "data.frame", row.names = c(NA,
4L)), `2` = structure(list(data = structure(list(event_type = c("response",
"response"), question_code = c("card:5", "mc:6"), response_value = c("50",
"60"), question_type = c("instruction", "q_select"), session = c("947ACA2D-D9A6-44AE-96CC-99B8A4271BDE",
"947ACA2D-D9A6-44AE-96CC-99B8A4271BDE"), more_data = structure(list(
    options = list(NULL, structure(list(name = c("hello", "goodbye",
    "aloha"), selected = c(FALSE, TRUE, FALSE), value = c("1",
    "2", "3")), .Names = c("name", "selected", "value"), class = "data.frame", row.names = c(NA,
    3L)))), .Names = "options", class = "data.frame", row.names = 1:2),
    response = c(NA, "goodbye")), .Names = c("event_type", "question_code",
"response_value", "question_type", "session", "more_data", "response"
), class = "data.frame", row.names = 1:2), metadata = structure(list(
    id = c("22139CB0-4FA5-4910-8272-D4EA0BFA89EA", "3651904C-4E49-4591-963D-D0CEB27FBC6D"
    ), pt = c("mark1", "mark1"), timestamp = c("2019-05-08T11:05:46-05:00",
    "2019-05-08T11:05:46-05:00")), .Names = c("id", "pt", "timestamp"
), class = "data.frame", row.names = 1:2)), .Names = c("data",
"metadata"), class = "data.frame", row.names = 1:2)), .Names = c("1",
"2"))
sourceCard <-
structure(list(card_type = "calculated", order = 2L, section = 1L,
    data = structure(list(title = NULL, text = NULL, icon = NULL,
        code = "calc1", color = NULL, tags = list(), i_title = NULL,
        i_text = NULL, serviceUrl = "http://localhost:3000",
        args = NULL), .Names = c("title", "text", "icon", "code",
    "color", "tags", "i_title", "i_text", "serviceUrl", "args"
    ))), .Names = c("card_type", "order", "section", "data"))
