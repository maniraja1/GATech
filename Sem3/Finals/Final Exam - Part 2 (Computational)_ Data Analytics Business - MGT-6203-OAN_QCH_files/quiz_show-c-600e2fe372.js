(window["canvasWebpackJsonp"]=window["canvasWebpackJsonp"]||[]).push([[2238],{E5fe:function(e,s,n){"use strict"
var a=n("ouhR")
var t=n.n(a)
const _={disableInputs(e){const s=t()("body"),n=t()("<div />",{class:"input_cover"})
n.on("mouseleave",(function(e){t()(this).remove()}))
t()(e).on("mouseenter",(function(e){const a=t()(this),_=n.clone(true)
_.css({height:a.height()+12,width:a.width()+12,position:"absolute",left:a.offset().left-6,top:a.offset().top-6,zIndex:15,background:"url(/images/blank.png) 0 0 repeat"})
s.append(_)}))},setWidths(e){t()(e||".answer input[type=text]").each((function(){t()(this).width(9.5*t()(this).val().length)}))}}
s["a"]=_},FtDy:function(e,s,n){"use strict"
var a=n("HGxv")
var t=n("8WeW")
Object(t["a"])(JSON.parse('{"ar":{"message_sent_9ff3a79d":"تم إرسال الرسالة!","message_students_for_course_name_e55f8077":"مراسلة الطلاب لـ %{course_name}","public_message_students":{"send_message":"إرسال رسالة"},"remove_student_from_recipients_4b206e35":"إزالة %{student} من قائمة المستلمين","send_message_6ccc90e8":"إرسال رسالة","sending_message_8ac5bc90":"جارٍ إرسال رسالة...","sending_message_failed_please_try_again_b53cc904":"فشل إرسال الرسالة، يرجى المحاولة مرة أخرى"},"ca":{"message_sent_9ff3a79d":"Missatge enviat!","message_students_for_course_name_e55f8077":"Envia un missatge als estudiants de %{course_name}","public_message_students":{"send_message":"Envia un missatge"},"remove_student_from_recipients_4b206e35":"Suprimeix %{student} dels destinataris","send_message_6ccc90e8":"Envia un missatge","sending_message_8ac5bc90":"S\'està enviant el missatge...","sending_message_failed_please_try_again_b53cc904":"No s\'ha pogut enviar el missatge, torneu-ho a provar"},"cy":{"message_sent_9ff3a79d":"Mae’r neges wedi’i hanfon!","message_students_for_course_name_e55f8077":"Anfon neges at Fyfyrwyr am %{course_name}","public_message_students":{"send_message":"Anfon Neges"},"remove_student_from_recipients_4b206e35":"Tynnu %{student} o’r rhestr derbynwyr","send_message_6ccc90e8":"Anfon Neges","sending_message_8ac5bc90":"Wrthi’n anfon neges...","sending_message_failed_please_try_again_b53cc904":"Wedi methu anfon neges, rhowch gynnig arall arni"},"da":{"message_sent_9ff3a79d":"Besked sendt!","message_students_for_course_name_e55f8077":"Send besked til studerende for %{course_name}","public_message_students":{"send_message":"Send besked"},"remove_student_from_recipients_4b206e35":"Fjern %{student} fra modtagere","send_message_6ccc90e8":"Send besked","sending_message_8ac5bc90":"Sender besked ...","sending_message_failed_please_try_again_b53cc904":"Afsendelse af besked mislykkedes, prøv igen"},"da-x-k12":{"message_sent_9ff3a79d":"Besked sendt!","message_students_for_course_name_e55f8077":"Send besked til elever for %{course_name}","public_message_students":{"send_message":"Send besked"},"remove_student_from_recipients_4b206e35":"Fjern %{student} fra modtagere","send_message_6ccc90e8":"Send besked","sending_message_8ac5bc90":"Sender besked ...","sending_message_failed_please_try_again_b53cc904":"Afsendelse af besked mislykkedes, prøv igen"},"de":{"message_sent_9ff3a79d":"Nachricht versandt!","message_students_for_course_name_e55f8077":"Studenten benachrichtigen wegen %{course_name}","public_message_students":{"send_message":"Nachricht senden"},"remove_student_from_recipients_4b206e35":"%{student} aus den Empfängern entfernen","send_message_6ccc90e8":"Nachricht senden","sending_message_8ac5bc90":"Nachricht wird gesendet ...","sending_message_failed_please_try_again_b53cc904":"Senden der Nachrichten ist fehlgeschlagen. Bitte erneut versuchen"},"el":{"message_sent_9ff3a79d":"Μήνυμα εστάλη!","message_students_for_course_name_e55f8077":"Αποστολή μηνύματος στους Σπουδαστές για το μάθημα %{course_name}","public_message_students":{"send_message":"Αποστολή Μηνύματος"},"remove_student_from_recipients_4b206e35":"Αφαίρεση του/της %{student} από τους παραλήπτες"},"en-AU":{"message_sent_9ff3a79d":"Message sent!","message_students_for_course_name_e55f8077":"Message students for %{course_name}","public_message_students":{"send_message":"Send Message"},"remove_student_from_recipients_4b206e35":"Remove %{student} from recipients","send_message_6ccc90e8":"Send Message","sending_message_8ac5bc90":"Sending Message...","sending_message_failed_please_try_again_b53cc904":"Sending Message Failed. Please try again."},"en-AU-x-unimelb":{"message_sent_9ff3a79d":"Message sent!","message_students_for_course_name_e55f8077":"Message students for %{course_name}","public_message_students":{"send_message":"Send Message"},"remove_student_from_recipients_4b206e35":"Remove %{student} from recipients","send_message_6ccc90e8":"Send Message","sending_message_8ac5bc90":"Sending Message...","sending_message_failed_please_try_again_b53cc904":"Sending Message Failed. Please try again."},"en-CA":{"message_sent_9ff3a79d":"Message sent!","message_students_for_course_name_e55f8077":"Message Students for %{course_name}","public_message_students":{"send_message":"Send Message"},"remove_student_from_recipients_4b206e35":"Remove %{student} from recipients","send_message_6ccc90e8":"Send Message","sending_message_8ac5bc90":"Sending Message...","sending_message_failed_please_try_again_b53cc904":"Sending Message Failed, please try again"},"en-GB":{"message_sent_9ff3a79d":"Message sent!","message_students_for_course_name_e55f8077":"Message students for %{course_name}","public_message_students":{"send_message":"Send message"},"remove_student_from_recipients_4b206e35":"Remove %{student} from recipients","send_message_6ccc90e8":"Send message","sending_message_8ac5bc90":"Sending message...","sending_message_failed_please_try_again_b53cc904":"Sending message failed. Please try again."},"en-GB-x-lbs":{"message_sent_9ff3a79d":"Message sent!","remove_student_from_recipients_4b206e35":"Remove %{student} from recipients","send_message_6ccc90e8":"Send message","sending_message_8ac5bc90":"Sending message...","sending_message_failed_please_try_again_b53cc904":"Sending message failed. Please try again."},"en-GB-x-ukhe":{"message_sent_9ff3a79d":"Message sent!","message_students_for_course_name_e55f8077":"Message students for %{course_name}","public_message_students":{"send_message":"Send message"},"remove_student_from_recipients_4b206e35":"Remove %{student} from recipients","send_message_6ccc90e8":"Send message","sending_message_8ac5bc90":"Sending message...","sending_message_failed_please_try_again_b53cc904":"Sending message failed. Please try again."},"es":{"message_sent_9ff3a79d":"¡Mensaje enviado!","message_students_for_course_name_e55f8077":"Mensaje a estudiantes para %{course_name}","public_message_students":{"send_message":"Enviar mensaje"},"remove_student_from_recipients_4b206e35":"Eliminar %{student} desde los destinatarios","send_message_6ccc90e8":"Enviar mensaje","sending_message_8ac5bc90":"Enviando mensaje...","sending_message_failed_please_try_again_b53cc904":"La presentación del mensaje ha fallado, inténtelo nuevamente"},"fa":{"message_sent_9ff3a79d":"پیام ارسال شد!","message_students_for_course_name_e55f8077":"ارسال پیام برای دانشجویان درس %{course_name}","public_message_students":{"send_message":"ارسال پیام"},"remove_student_from_recipients_4b206e35":"حذف %{student} از گیرنده ها","send_message_6ccc90e8":"ارسال پیام","sending_message_8ac5bc90":"در حال ارسال پیام...","sending_message_failed_please_try_again_b53cc904":"ارسال پیام انجام نشد، لطفا دوباره سعی کنید"},"fi":{"message_sent_9ff3a79d":"Viesti lähetetty!","message_students_for_course_name_e55f8077":"Lähetä viesti opiskelijoille, jotka%{course_name}","public_message_students":{"send_message":"Lähetä viesti"},"remove_student_from_recipients_4b206e35":"Poista %{student} vastaanottajilta","send_message_6ccc90e8":"Lähetä viesti","sending_message_8ac5bc90":"Lähetetään viestiä...","sending_message_failed_please_try_again_b53cc904":"Viestin lähetys epäonnistui, yritä uudelleen"},"fr":{"message_sent_9ff3a79d":"Message envoyé !","message_students_for_course_name_e55f8077":"Envoyer un message aux étudiants pour %{course_name}","public_message_students":{"send_message":"Envoyer message"},"remove_student_from_recipients_4b206e35":"Supprimer %{student} des destinataires","send_message_6ccc90e8":"Envoyer message","sending_message_8ac5bc90":"Envoi du message...","sending_message_failed_please_try_again_b53cc904":"L’envoi du message a échoué, veuillez réessayer"},"fr-CA":{"message_sent_9ff3a79d":"Message envoyé!","message_students_for_course_name_e55f8077":"Envoyer un message aux étudiants pour le cours %{course_name}","public_message_students":{"send_message":"Envoyer un message"},"remove_student_from_recipients_4b206e35":"Retirer %{student} des destinataires","send_message_6ccc90e8":"Envoyer un message","sending_message_8ac5bc90":"Envoi du message...","sending_message_failed_please_try_again_b53cc904":"L’envoi du message a échoué, veuillez réessayer"},"he":{"message_sent_9ff3a79d":"הודעה נשלחה!","message_students_for_course_name_e55f8077":"הודעה לתלמידי הקורס %{course_name}","public_message_students":{"send_message":"שליחת הודעה"},"remove_student_from_recipients_4b206e35":"הסרת %{student} מרשימת הנמענים","send_message_6ccc90e8":"שליחת הודעה","sending_message_8ac5bc90":"שולח הודעה...","sending_message_failed_please_try_again_b53cc904":"משלוח הודעה נכשל, יש לנסות שוב"},"ht":{"message_sent_9ff3a79d":"Mesaj ale!","message_students_for_course_name_e55f8077":"Voye mesaj bay Elèv pou %{course_name}","public_message_students":{"send_message":"Voye Mesaj"},"remove_student_from_recipients_4b206e35":"Elimine %{student} nan destinatè yo","send_message_6ccc90e8":"Voye Mesaj","sending_message_8ac5bc90":"Voye Mesaj...","sending_message_failed_please_try_again_b53cc904":"Echèk Anvwa mesaj, tanpri eseye ankò"},"hu":{"message_sent_9ff3a79d":"Az üzenet elküldve!","message_students_for_course_name_e55f8077":"Üzenet a következő kurzus hallgatóinak: %{course_name}","public_message_students":{"send_message":"Üzenetküldés"},"remove_student_from_recipients_4b206e35":"%{student} nevű címzett eltávolítása","send_message_6ccc90e8":"Üzenetküldés","sending_message_8ac5bc90":"Üzenet küldése...","sending_message_failed_please_try_again_b53cc904":"Az üzenet küldése sikertelen, kérjük, próbálja újra!"},"hy":{"message_sent_9ff3a79d":"Հաղորդագրությունն ուղարկված է","message_students_for_course_name_e55f8077":"Ուղարկել հաղորդագրություն %{course_name} ունկնդիրներին","public_message_students":{"send_message":"Ուղարկել հաղորդագրություն"},"remove_student_from_recipients_4b206e35":"Ջնջել %{student} -ին ստացողների ցանկից"},"is":{"message_sent_9ff3a79d":"Skilaboð send!","message_students_for_course_name_e55f8077":"Senda nemendum í námskeiðinu %{course_name} skilaboð","public_message_students":{"send_message":"Senda skilaboð"},"remove_student_from_recipients_4b206e35":"Fjarlægja %{student} úr viðtakendum","send_message_6ccc90e8":"Senda skilaboð","sending_message_8ac5bc90":"Sendi skilaboð ...","sending_message_failed_please_try_again_b53cc904":"Ekki tókst að senda skilaboð, reyndu aftur"},"it":{"message_sent_9ff3a79d":"Messaggio inviato.","message_students_for_course_name_e55f8077":"Invia messaggio agli studenti per %{course_name}","public_message_students":{"send_message":"Invia messaggio"},"remove_student_from_recipients_4b206e35":"Rimuovi %{student} dai destinatari","send_message_6ccc90e8":"Invia messaggio","sending_message_8ac5bc90":"Invio messaggio in corso...","sending_message_failed_please_try_again_b53cc904":"Invio messaggio non riuscito. Riprova"},"ja":{"message_sent_9ff3a79d":"メッセージが送信されました!","message_students_for_course_name_e55f8077":"%{course_name}の受講生にメッセージを送る","public_message_students":{"send_message":"メッセージを送信"},"remove_student_from_recipients_4b206e35":"受信者から%{student}を削除する","send_message_6ccc90e8":"メッセージを送信","sending_message_8ac5bc90":"メッセージを送信しています...","sending_message_failed_please_try_again_b53cc904":"メッセージの送信に失敗しました。もう一度やり直してください"},"ko":{"message_students_for_course_name_e55f8077":"%{course_name}의 학생 관리","public_message_students":{"send_message":"메시지 보내기"}},"mi":{"message_sent_9ff3a79d":"Karere kua tukuna!","message_students_for_course_name_e55f8077":"Karere ngā ākonga mō te %{course_name}","public_message_students":{"send_message":"Tukua Karere"},"remove_student_from_recipients_4b206e35":"Tangohia %{student} i ngā kaiwhiwhi","send_message_6ccc90e8":"Tukua Karere","sending_message_8ac5bc90":"Te tuku Karere ...","sending_message_failed_please_try_again_b53cc904":"Te tuku Karere rahua, tēnā ngana anō"},"nb":{"message_sent_9ff3a79d":"Melding sendt!","message_students_for_course_name_e55f8077":"Send beskjed til studenter i %{course_name}","public_message_students":{"send_message":"Send melding"},"remove_student_from_recipients_4b206e35":"Fjerne %{student} fra mottakerlisten","send_message_6ccc90e8":"Send melding","sending_message_8ac5bc90":"Sender melding...","sending_message_failed_please_try_again_b53cc904":"Sending av melding feilet, vennligst prøv igjen"},"nb-x-k12":{"message_sent_9ff3a79d":"Melding sendt!","message_students_for_course_name_e55f8077":"Send beskjed til elever i %{course_name}","public_message_students":{"send_message":"Send melding"},"remove_student_from_recipients_4b206e35":"Fjerne %{student} fra mottakerlisten","send_message_6ccc90e8":"Send melding","sending_message_8ac5bc90":"Sender melding...","sending_message_failed_please_try_again_b53cc904":"Sending av melding feilet, vennligst prøv igjen"},"nl":{"message_sent_9ff3a79d":"Bericht verzonden!","message_students_for_course_name_e55f8077":"Bericht naar cursisten voor %{course_name}","public_message_students":{"send_message":"Bericht versturen"},"remove_student_from_recipients_4b206e35":"%{student} uit geadresseerden verwijderen","send_message_6ccc90e8":"Bericht versturen","sending_message_8ac5bc90":"Bericht aan het versturen...","sending_message_failed_please_try_again_b53cc904":"Inleveren van bericht mislukt, probeer het opnieuw"},"nn":{"message_sent_9ff3a79d":"Meldinga er send","message_students_for_course_name_e55f8077":"Send melding til studentar i %{course_name}","public_message_students":{"send_message":"Send melding"},"remove_student_from_recipients_4b206e35":"Fjern %{student} frå mottakarar","send_message_6ccc90e8":"Send melding","sending_message_8ac5bc90":"Sender melding...","sending_message_failed_please_try_again_b53cc904":"Sending av melding mislukkast, prøv på nytt seinare"},"pl":{"message_sent_9ff3a79d":"Wiadomość wysłana!","message_students_for_course_name_e55f8077":"Wiadomość dla uczestników dot %{course_name}","public_message_students":{"send_message":"Wyślij wiadomość"},"remove_student_from_recipients_4b206e35":"Usuń %{student} z odbiorców","send_message_6ccc90e8":"Wyślij wiadomość","sending_message_8ac5bc90":"Wysyłanie wiadomości...","sending_message_failed_please_try_again_b53cc904":"Wysyłanie wiadomości nie powiodło się, spróbuj ponownie"},"pt":{"message_sent_9ff3a79d":"Mensagem enviada!","message_students_for_course_name_e55f8077":"Mensagem a alunos para %{course_name}","public_message_students":{"send_message":"Enviar Mensagem"},"remove_student_from_recipients_4b206e35":"Remover %{student} dos destinatários","send_message_6ccc90e8":"Enviar Mensagem","sending_message_8ac5bc90":"Enviando mensagem...","sending_message_failed_please_try_again_b53cc904":"Falha no envio da mensagem, tente novamente"},"pt-BR":{"message_sent_9ff3a79d":"Mensagem enviada!","message_students_for_course_name_e55f8077":"Enviar mensagem aos alunos para %{course_name}","public_message_students":{"send_message":"Enviar mensagem"},"remove_student_from_recipients_4b206e35":"Remover %{student} de destinatários","send_message_6ccc90e8":"Enviar mensagem","sending_message_8ac5bc90":"Enviando mensagem...","sending_message_failed_please_try_again_b53cc904":"Falha no envio da mensagem, tente novamente"},"ru":{"message_sent_9ff3a79d":"Сообщение отправлено!","message_students_for_course_name_e55f8077":"Отправить сообщение студентам для %{course_name}","public_message_students":{"send_message":"Отправить сообщение"},"remove_student_from_recipients_4b206e35":"Удалить %{student} из получателей","send_message_6ccc90e8":"Отправить сообщение","sending_message_8ac5bc90":"Отправка сообщения...","sending_message_failed_please_try_again_b53cc904":"Ошибка отправки сообщения, повторите попытку"},"sl":{"message_sent_9ff3a79d":"Sporočilo je poslano.","remove_student_from_recipients_4b206e35":"Odstrani študenta %{student} iz prejemnikov","send_message_6ccc90e8":"Pošlji sporočilo","sending_message_8ac5bc90":"Pošiljanje sporočila ...","sending_message_failed_please_try_again_b53cc904":"Pošiljanje sporočila ni uspelo, poskusite znova."},"sv":{"message_sent_9ff3a79d":"Meddelandet har skickats!","message_students_for_course_name_e55f8077":"Meddela studenter om %{course_name}","public_message_students":{"send_message":"Skicka meddelande"},"remove_student_from_recipients_4b206e35":"Ta bort %{student} från mottagare","send_message_6ccc90e8":"Skicka meddelande","sending_message_8ac5bc90":"Skickar meddelande ...","sending_message_failed_please_try_again_b53cc904":"Meddelandet skickades inte, försök igen"},"sv-x-k12":{"message_sent_9ff3a79d":"Meddelandet har skickats!","message_students_for_course_name_e55f8077":"Meddela elever om %{course_name}","public_message_students":{"send_message":"Skicka meddelande"},"remove_student_from_recipients_4b206e35":"Ta bort %{student} från mottagare","send_message_6ccc90e8":"Skicka meddelande","sending_message_8ac5bc90":"Skickar meddelande ...","sending_message_failed_please_try_again_b53cc904":"Meddelandet skickades inte, försök igen"},"tr":{"message_sent_9ff3a79d":"Mesaj gönderildi!","message_students_for_course_name_e55f8077":"%{course_name} dersi Öğrencilerine Mesaj Gönder","public_message_students":{"send_message":"Mesaj Gönder"},"send_message_6ccc90e8":"Mesaj Gönder","sending_message_8ac5bc90":"Mesajınız Gönderiliyor...","sending_message_failed_please_try_again_b53cc904":"Mesah gönderimi başarısız, lütfen tekrar deneyin"},"uk":{"message_sent_9ff3a79d":"Повідомлення відправлено!","message_students_for_course_name_e55f8077":"Повідомлення студентів для %{course_name}","public_message_students":{"send_message":"Відправити повідомлення"},"remove_student_from_recipients_4b206e35":"Видалити %{student} зі списку отримувачів","send_message_6ccc90e8":"Відправити повідомлення","sending_message_8ac5bc90":"Надіслати повідомлення...","sending_message_failed_please_try_again_b53cc904":"Не вдалося надіслати повідомлення, спробуйте ще раз"},"zh-Hans":{"message_sent_9ff3a79d":"邮件已发送！","message_students_for_course_name_e55f8077":"给学生发送%{course_name}的消息","public_message_students":{"send_message":"发送消息"},"remove_student_from_recipients_4b206e35":"从收件人中移除 %{student}","send_message_6ccc90e8":"发送消息","sending_message_8ac5bc90":"发送信息...","sending_message_failed_please_try_again_b53cc904":"发送信息失败，请再尝试"},"zh-Hant":{"message_sent_9ff3a79d":"訊息已發送！","message_students_for_course_name_e55f8077":"針對 %{course_name} 通知學生","public_message_students":{"send_message":"發送訊息"},"remove_student_from_recipients_4b206e35":"從收件人中刪除 %{student}","send_message_6ccc90e8":"傳送訊息","sending_message_8ac5bc90":"正在發送郵件...","sending_message_failed_please_try_again_b53cc904":"發送郵件失敗，請再試一次"}}'))
n("jQeR")
n("0sPK")
var _=a["default"].scoped("public_message_students")
var i=n("ouhR")
var d=n.n(i)
var c=n("plYi")
var r=n("FdVj")
n("Dhso")
n("ESjL")
n("aq8L")
let o={}
function m(){const e=l()
u(0==e.find("#body").val().length||0==e.find(".student:not(.blank):visible").length)}window.messageStudents=function(e){const s=l()
o=e
s.find(".message_types").empty()
for(let n=0,a=e.options.length;n<a;n++){const a=d()("<option/>")
const t=e.options[n]
a.val(n).text(t.text)
s.find(".message_types").append(a)}const n=e.title,a=s.find("ul li.blank:first"),t=s.find("ul"),i={}
s.find("ul li:not(.blank)").remove()
const r=e.students.slice()
r.sort(c["a"].byKey("sortableName"))
for(let e=0;e<r.length;e++){const s=r[e]
const n=a.clone(true).removeClass("blank")
n.find(".name").text(s.name)
n.find(".score").text(s.score)
const c=_.t("Remove %{student} from recipients",{student:s.name})
const o=n.find(".remove-button")
o.attr("title",c).append(d()("<span class='screenreader-only'></span>").text(c))
o.click((function(e){e.preventDefault()
const s=d()(this).closest("li")
s.hide("fast",m)
const n=s.nextAll(":visible:first")
n.length?d()("button",n).focus():d()("#message_assignment_recipients #subject").focus()}))
n.data("id",s.id)
n.user_data=s
t.append(n.show())
i[s.id]=n}t.show()
const g=_.t("Message Students for %{course_name}",{course_name:n})
s.data("students_hash",i),s.find(".asset_title").text(n)
s.find(".out_of").showIf(null!=e.points_possible)
s.find(".send_button").text(_.t("send_message","Send Message"))
s.find(".points_possible").text(_.n(e.points_possible))
s.find("[name=context_code]").val(e.context_code)
s.find("textarea").val("")
s.find("select")[0].selectedIndex=0
s.find("select").change()
s.dialog({width:600,modal:true,open:(e,n)=>{s.closest(".ui-dialog").attr("role","dialog").attr("aria-label",g)},close:(e,n)=>{s.closest(".ui-dialog").removeAttr("role").removeAttr("aria-label")}}).dialog("open").dialog("option","title",g).on("dialogclose",e.onClose)}
d()(document).ready(()=>{const e=l()
e.find("button").click(e=>{const s=d()(e.target)
if(s.hasClass("disabled")){e.preventDefault()
e.stopPropagation()}})
d()("#message_assignment_recipients").formSubmit({processData(e){const s=[]
d()(this).find(".student:visible").each((function(){s.push(d()(this).data("id"))}))
if(0==s.length)return false
e.recipients=s.join(",")
return e},beforeSubmit(e){g(true)
d()(this).find(".send_button").text(_.t("Sending Message..."))},success(e){d.a.flashMessage(_.t("Message sent!"))
g(false)
d()(this).find(".send_button").text(_.t("Send Message"))
d()("#message_students_dialog").dialog("close")},error(e){g(false)
d()(this).find(".send_button").text(_.t("Sending Message Failed, please try again"))}})
const s=function(){const s=parseInt(e.find("select").val(),10)||0
const n=o.options[s]
const a=e.data("students_hash")
let t=r["a"].parse(e.find(".cutoff_score").val())
isNaN(t)&&(t=null)
const _=Object.values(a)
let i=[]
a&&(n&&n.callback?i=n.callback.call(window.messageStudents,t,_):o.callback&&(i=o.callback.call(window.messageStudents,n.text,t,_)))
o.subjectCallback&&e.find("[name=subject]").val(o.subjectCallback(n.text,t))
e.find(".cutoff_holder").showIf(n.cutoff)
e.find(".student_list").toggleClass("show_score",!!(n.cutoff||n.score))
g(0===i.length)
const d=new Set(i)
Object.entries(a).forEach(([e,s])=>{s.showIf(d.has(e))})}
const n=function(){e.dialog("close")}
e.find(".cancel_button").click(n)
e.find("select").change(s).change(m)
e.find(".cutoff_score").bind("change blur keyup",s).bind("change blur keyup",m)
e.find("#body").bind("change blur keyup",m)})
function g(e,s){null==s&&(s=l().find("button"))
s.toggleClass("disabled",e).attr("aria-disabled",e)}function u(e){g(e,l().find(".send_button"))}function l(){return d()("#message_students_dialog")}s["a"]=messageStudents},jRRY:function(e,s,n){"use strict"
n.r(s)
var a=n("ouhR")
var t=n.n(a)
var _=n("E5fe")
var i=n("An8g")
var d=n("JtOd")
n("q1tI")
var c=n("i8i4")
var r=n.n(c)
var o=n("pcEE")
var m=n("hFJA")
var g=n("3l1C")
var u=n("Vovh")
var l=n("EwH5")
var f=n("6dnZ")
n("7svE")
n("ESjL")
n("897F")
n("r2Yr")
n("aq8L")
n("sXof")
n("FtDy")
var b=n("VrCy")
var p=n("QbG7")
var v=n("Nuch")
t()(document).ready((function(){ENV.QUIZ_SUBMISSION_EVENTS_URL&&Object(l["a"])(true)
t()("#preview_quiz_button").click(e=>{t()("#js-sequential-warning-dialogue div a").attr("href",t()("#preview_quiz_button").attr("href"))})
function e(e){return t()("#quiz_details").length?e():t.a.get(ENV.QUIZ_DETAILS_URL,s=>{t()("#quiz_details_wrapper").html(s)
e()})}const s=new m["a"]
s.applyArrows()
if(!t()(".allow-inputs").length){_["a"].disableInputs("[type=radio], [type=checkbox]")
_["a"].setWidths()}t()("form.edit_quizzes_quiz").on("submit",(function(e){e.preventDefault()
e.stopImmediatePropagation()
t()(this).find(".loading").removeClass("hidden")
const s=t()(this).serializeArray()
const n=t()(this).attr("action")
t.a.ajax({url:n,data:s,type:"POST",success(){t()(".edit_quizzes_quiz").parents(".alert").hide()}})}))
t()(".delete_quiz_link").click((function(e){e.preventDefault()
let s=d["a"].t("confirms.delete_quiz","Are you sure you want to delete this quiz?")
const n=parseInt(t()("#quiz_details_wrapper").data("submitted-count"))
n>0&&(s+="\n\n"+d["a"].t("confirms.delete_quiz_submissions_warning",{one:"Warning: 1 student has already taken this quiz. If you delete it, any completed submissions will be deleted and no longer appear in the gradebook.",other:"Warning: %{count} students have already taken this quiz. If you delete it, any completed submissions will be deleted and no longer appear in the gradebook."},{count:n}))
t()("nothing").confirmDelete({url:t()(this).attr("href"),message:s,success(){window.location.href=ENV.QUIZZES_URL}})}))
let n=false
t()(".quiz_details_link").click(s=>{s.preventDefault()
t()("#quiz_details_wrapper").disableWhileLoading(e(()=>{const e=t()("#quiz_details_text")
t()("#quiz_details").slideToggle()
n?ENV.IS_SURVEY?e.text(d["a"].t("links.show_student_survey_results","Show Student Survey Results")):e.text(d["a"].t("links.show_student_quiz_results","Show Student Quiz Results")):ENV.IS_SURVEY?e.text(d["a"].t("links.hide_student_survey_results","Hide Student Survey Results")):e.text(d["a"].t("links.hide_student_quiz_results","Hide Student Quiz Results"))
n=!n}))})
t()(".message_students_link").click(s=>{s.preventDefault()
e(()=>{const e=ENV.QUIZ_SUBMISSION_LIST
const s=e.UNSUBMITTED_STUDENTS
const n=e.SUBMITTED_STUDENTS
const a=d["a"].t("students_who_have_taken_the_quiz","Students who have taken the quiz")
const t=d["a"].t("students_who_have_not_taken_the_quiz","Students who have NOT taken the quiz")
const _=new o["a"]({context:ENV.QUIZ.title,recipientGroups:[{name:a,recipients:n},{name:t,recipients:s}]})
_.open()})})
function a(e,s=true){e&&e.preventDefault()
r.a.render(Object(i["a"])(p["a"],{open:s,sourceCourseId:ENV.COURSE_ID,contentShare:{content_type:"quiz",content_id:ENV.QUIZ.id},onDismiss:()=>{a(null,false)
t()(".al-trigger").focus()}}),document.getElementById("direct-share-mount-point"))}t()(".direct-share-send-to-menu-item").click(a)
function c(e,s=true){e&&e.preventDefault()
r.a.render(Object(i["a"])(v["a"],{open:s,sourceCourseId:ENV.COURSE_ID,contentSelection:{quizzes:[ENV.QUIZ.id]},onDismiss:()=>{c(null,false)
t()(".al-trigger").focus()}}),document.getElementById("direct-share-mount-point"))}t()(".direct-share-copy-to-menu-item").click(c)
t()("#let_students_take_this_quiz_button").ifExists((function(e){const s=t()("#unlock_for_how_long_dialog")
e.click(()=>{s.dialog("open")
return false})
const n=t()(this).find(".datetime_field")
s.dialog({autoOpen:false,modal:true,resizable:false,width:400,buttons:{Unlock(){t()("#quiz_unlock_form").append(t()(this).dialog("destroy")).find("#quiz_lock_at").val(n.data("iso8601")).end().submit()}}})
n.datetime_field()}))
t()("#lock_this_quiz_now_link").ifExists(e=>{e.click(e=>{e.preventDefault()
t()("#quiz_lock_form").submit()})})
t()("ul.page-action-list").find("li").length>0&&t()("ul.page-action-list").show()
t()("#publish_quiz_form").formSubmit({beforeSubmit(e){t()(this).find("button").attr("disabled",true).text(d["a"].t("buttons.publishing","Publishing..."))},success(e){t()(this).find("button").text(d["a"].t("buttons.already_published","Published!"))
location.reload()}})
const h=t()("#quiz-publish-link")
const k=new g["a"](t.a.extend(ENV.QUIZ,{unpublishable:!h.hasClass("disabled")}))
const y=new u["a"]({model:k,el:h})
const S=function(){location.href=location.href}
y.on("publish",S)
y.on("unpublish",S)
y.render()
const w=document.getElementById("crs-graphs")
const E=document.getElementById("not_right_side")
f["default"].init(w,E)
t()("#assignment_external_tools").length&&b["a"].attach(t()("#assignment_external_tools")[0],"assignment_view",parseInt(ENV.COURSE_ID,10),parseInt(ENV.QUIZ.assignment_id,10))}))
n("VZGD")
n("fY8A")
var h=n("40dz")
const k=new h["default"]
k.init({itemType:"quiz",page:"show"})
t()(()=>{_["a"].setWidths()
t()(".answer input[type=text]").each((function(){t()(this).width(9.5*(t()(this).val().length||11))}))
t()(".download_submissions_link").click((function(e){e.preventDefault()
INST.downloadSubmissions(t()(this).attr("href"))}))
if(ENV.SUBMISSION_VERSIONS_URL&&!ENV.IS_SURVEY){const e=t()("#quiz-submission-version-table")
e.css({height:"100px"})
const s=t.a.get(ENV.SUBMISSION_VERSIONS_URL,s=>{e.html(s)
e.css({height:"auto"})})
e.disableWhileLoading(s)}t()("#module_sequence_footer").moduleSequenceFooter({courseID:ENV.COURSE_ID,assetType:"Quiz",assetID:ENV.QUIZ.id,location:location})})}}])

//# sourceMappingURL=quiz_show-c-600e2fe372.js.map