unit advmphp;
{$I TMSDEFS.Inc}

interface

uses
   Classes, AdvMemo,
{$IFDEF TMSCLX}
   QGraphics
{$ENDIF}
{$IFNDEF TMSCLX}
   Graphics
{$ENDIF}
   ;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
   TAdvPHPMemoStyler = class(TAdvCustomMemoStyler)
   public
     constructor Create(AOwner: TComponent); override;
   published
     property BlockStart;
     property BlockEnd;
     property LineComment;
     property MultiCommentLeft;
     property MultiCommentRight;
     property CommentStyle;
     property NumberStyle;
     property AllStyles;
     property AutoCompletion;
     property HintParameter;
     property HexIdentifier;

    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
    property RegionDefinitions;
   end;

implementation

//uses Graphics;

constructor TAdvPHPMemoStyler.Create(AOwner: TComponent);
var
   itm:TElementStyle;
begin
   inherited;

   Description := 'PHP and E107';
   StylerName := Description;
   Extensions := 'php';

   BlockStart := '{';
   BlockEnd := '}';

   LineComment := '//';
   MultiCommentLeft := '/*';
   MultiCommentRight := '*/';
   CommentStyle.TextColor := clNavy;
   CommentStyle.BkColor := clWhite;
   CommentStyle.Style := [fsItalic];

   NumberStyle.TextColor := clFuchsia;
   NumberStyle.BkColor := clWhite;
   NumberStyle.Style := [fsBold];

   itm := AllStyles.Add;
   itm.Info := 'PHP Standard';
   itm.Font.Color := clGreen;
   itm.Font.Style := [fsBold];
   with itm.KeyWords do
   begin
     Add('BREAK');
     Add('CLASS');
     Add('CONTINUE');
     Add('DECLARE');
     Add('DEFINE');
     Add('DO');
     Add('ECHO');
     Add('ELSE');
     Add('ELSEIF');
     Add('FOR');
     Add('FOREACH');
     Add('FUNCTION');
     Add('IF');
     Add('INCLUDE');
     Add('INCLUDE_ONCE');
     Add('PRINT');
     Add('REQUIRE');
     Add('REQUIRE_ONCE');
     Add('RETURN');
     Add('SWITCH');
     Add('WHILE');
   end;(* with *)

   itm := AllStyles.Add;
   itm.Info := 'E107';
   itm.Font.Color := clOlive;
   itm.Font.Style := [fsBold];
   with itm.KeyWords do
   begin
     Add('AVATAR');
     Add('BAN');
     Add('check_class');
     Add('convert_date');
     Add('dbError');
     Add('db_Close');
     Add('db_Connect');
     Add('db_Count');
     Add('db_Delete');
     Add('db_Fetch');
     Add('db_Fieldname');
     Add('db_Insert');
     Add('db_Rows');
     Add('db_Select');
     Add('db_Select_gen');
     Add('db_Update');
     Add('delete_item');
     Add('delete_poll');
     Add('editparse');
     Add('edit_item');
     Add('enter_comment');
     Add('flood');
     Add('form_button');
     Add('form_checkbox');
     Add('form_comment');
     Add('form_file');
     Add('form_hidden');
     Add('form_open');
     Add('form_option');
     Add('form_password');
     Add('form_radio');
     Add('form_select_close');
     Add('form_select_open');
     Add('form_text');
     Add('form_textarea');
     Add('getip');
     Add('getperms');
     Add('gettime');
     Add('init_session');
     Add('online');
     Add('preview');
     Add('render_comment');
     Add('render_newsitem');
     Add('render_poll');
     Add('r_emote');
     Add('save_prefs');
     Add('sendemail');
     Add('set_error_handler');
     Add('submit_item');
     Add('submit_poll');
     Add('tablerender');
     Add('cache_fname');
     Add('tp');
     Add('tpa');
     Add('update_cache');
     Add('userlogin');
     Add('e107cache_page_md5');
     Add('set');
     Add('set_cache');
     Add('retrieve_cache');
     Add('clear');
     Add('delete');
     Add('checkvalidtheme');
     Add('e107_parse');
     Add('textparse');
     Add('editparse');
     Add('tpj');
   end;(* with *)

   //------------Simple Quote ' '----------------
   itm := AllStyles.Add;
   itm.StyleType := stBracket;
   itm.Info := 'Simple Quote';
   itm.Font.Color := clBlue;
   itm.Font.Style := [];
   itm.BracketStart := #39;
   itm.BracketEnd := #39;
   //------------Double Quote " "----------------
   itm := AllStyles.Add;
   itm.StyleType := stBracket;
   itm.Info := 'Double Quote';
   itm.Font.Color := clBlue;
   itm.Font.Style := [];
   itm.BracketStart := '"';
   itm.BracketEnd := '"';
   //----------SYMBOL --------------
   itm := AllStyles.Add;
   itm.StyleType := stSymbol;
   itm.Info := 'Symbols Delimiters';
   itm.Font.Color := clred;
   itm.Font.Style := [];
   itm.Symbols := #32+',;:.()&{}[]=-*/^%<>#'+#13+#10;

   with HintParameter.Parameters do
   begin
     { TODO 1 -ctodo : miten int ja void juttujen k‰sittely. }
     Add('print( string arg ) : int;');
     Add('printf ( string format [, mixed args]);');

     (* E107-SPESIFIC *)
     Add('Avatar($user_image);');
     Add('ban();');
     Add('check_class($var, $userclass=USERCLASS);');
     Add('convert_date($datestamp, $mode="long");');
     Add('dbError($Form);');
     Add('db_Close();');
     Add('db_Connect($mySQLserver, $mySQLuser, $mySQLpassword, $mySQLdefaultdb);');
     Add('db_Count($table, $fields="(*)", $arg="");');
     Add('db_Delete($table, $arg="");');
     Add('db_Fetch();');
     Add('db_Fieldname($offset);');
     Add('db_Insert($table, $arg);');
     Add('db_Rows();');
     Add('db_Select($table, $fields="*", $arg="", $mode="default");');
     Add('db_Select_gen($arg);');
     Add('db_Update($table, $arg);');
     Add('delete_item($news_id);');
     Add('delete_poll($existing);');
     Add('editparse($text, $mode="off");');
     Add('edit_item($existing);');
     Add('enter_comment($author_name, $comment, $table, $id);');
     Add('flood($table, $orderfield);');
     Add('form_button($form_type, $form_name, $form_value, $form_js="", $form_image="", $form_tooltip="");');
     Add('form_checkbox($form_name, $form_value, $form_checked=0, $form_tooltip="");');
     Add('form_comment();');
     Add('form_file($form_name, $form_size, $form_tooltip="");');
     Add('form_hidden($form_name, $form_value);');
     Add('form_open($form_method, $form_action, $form_name="", $form_target= "", $form_enctype="");');
     Add('form_option($form_method, $form_action, $form_name="", $form_target = "", $form_enctype="");');
     Add('form_password($form_name, $form_size, $form_value, $form_maxlength, $form_class="tbox", $form_readonly="", $form_tooltip="");');
     Add('form_radio($form_name, $form_value, $form_checked=0, $form_tooltip="");');
     Add('form_select_close();');
     Add('form_select_open($form_name);');
     Add('form_text($form_name, $form_size, $form_value, $form_maxlength, $form_class="tbox", $form_readonly="", $form_tooltip="");');
     Add('form_textarea($form_name, $form_columns, $form_rows, $form_value, $form_js="", $form_style="", $form_wrap="", $form_readonly="", $form_tooltip="");');
     Add('getip();');
     Add('getperms($arg, $ap = ADMINPERMS);');
     Add('gettime();');
     Add('init_session();');
     Add('online($page);');
     Add('preview($news_id, $news_title, $news_body, $news_extended, $news_source, $news_url, $cat_id, $allow_comments, $active_start, $active_end, $news_active);');
     Add('render_comment($row);');
     Add('render_newsitem($news_id, $news_title, $news_body, $news_extended, $news_source, $news_url, $news_author, $comment_total, $category_id, $datestamp, $allow_comments, $active_start, $active_end, $news_active, $modex="");');
     Add('render_poll($poll_id, $poll_question, $poll_option, $votes, $mode, $type="menu");');
     Add('r_emote();');
     Add('save_prefs($table = "core");');
     Add('sendemail($send_to, $subject, $message);');
     Add('set_error_handler($type, $message, $file, $line, $context);');
     Add('submit_item($news_id, $news_title, $news_body, $news_extended, $news_source, $news_url, $category_id, $allow_comments, $news_start, $news_end, $news_active);');
     Add('submit_poll($poll_id, $poll_name, $poll_option, $activate, $id=0, $ref="menu");');
     Add('tablerender($caption, $text, $mode="default");');
     Add('cache_fname($query);');
     Add('tp($text, $mode="off");');
     Add('tpa($text, $mode="off");');
     Add('update_cache($table);');
     Add('userlogin($username, $userpass, $autologin);');
     Add('e107cache_page_md5();');
     Add('set($query, $text);');
     Add('set_cache();');
     Add('retrieve_cache($query);');
     Add('clear();');
     Add('delete($dir, $pattern);');
     Add('checkvalidtheme($theme_to_check);');
     Add('e107_parse($text,$referrer);');
     Add('textparse();');
     Add('editparse($text, $mode="off");');
     Add('tpj();');
   end;

   with AutoCompletion do
   begin
     { TODO 1 -ctodo : miten pit‰‰ lis‰t‰ editori asetuksissa. }
{    AddObject('int status;', TObject( ttVar ));
     AddObject('void Auto;', TObject( ttProc ));}
     AddObject('int Print(string arg);', TObject( ttFunc));
{    AddObject('void Clear;', TObject( ttFunc));}
     AddObject('Printf', TObject( ttProc ));

     (* E107-SPECIFIC *)
     AddObject('Avatar($user_image);',TObject( ttProc ));
     AddObject('ban();',TObject( ttProc ));
     AddObject('cache_fname($query);', TObject( ttFunc ));
     AddObject('check_class($var,$userclass=USERCLASS);',TObject( ttProc ));
     AddObject('checkvalidtheme($theme_to_check);', TObject( ttFunc ));
     AddObject('clear();', TObject( ttFunc ));
     AddObject('convert_date($datestamp,$mode="long");',TObject( ttFunc ));
     AddObject('db_Close();',TObject( ttProc ));
     AddObject('db_Connect($mySQLserver,$mySQLuser,$mySQLpassword,$mySQLdefaultdb);',TObject( ttFunc ));
     AddObject('db_Count($table,$fields="(*)",$arg="");',TObject( ttFunc ));
     AddObject('db_Delete($table,$arg="");',TObject( ttProc ));
     AddObject('db_Fetch();',TObject( ttFunc ));
     AddObject('db_Fieldname($offset);',TObject( ttFunc ));
     AddObject('db_Insert($table,$arg);',TObject( ttProc ));
     AddObject('db_Rows();',TObject( ttFunc ));
     AddObject('db_Select($table,$fields="*",$arg="",$mode="default");',TObject( ttFunc ));
     AddObject('db_Select_gen($arg);',TObject( ttFunc ));
     AddObject('db_Update($table,$arg);',TObject( ttProc ));
     AddObject('dbError($Form);',TObject( ttFunc ));
     AddObject('delete($dir,$pattern);', TObject( ttFunc ));
     AddObject('delete_item($news_id);',TObject( ttProc ));
     AddObject('delete_poll($existing);',TObject( ttProc ));
     AddObject('e107_parse($text,$referrer);', TObject( ttFunc ));
     AddObject('e107cache_page_md5();', TObject( ttFunc ));
     AddObject('edit_item($existing);',TObject( ttProc ));
     AddObject('editparse($text,$mode="off");', TObject( ttFunc ));
     AddObject('enter_comment($author_name,$comment,$table,$id);',TObject( ttProc ));
     AddObject('flood($table,$orderfield);',TObject( ttFunc ));
     AddObject('form_button($form_type,$form_name,$form_value,$form_js="",$form_image="",$form_tooltip="");',TObject( ttProc ));
     AddObject('form_checkbox($form_name,$form_value,$form_checked=0,$form_tooltip="");',TObject( ttProc ));
     AddObject('form_comment();', TObject( ttFunc ));
     AddObject('form_file($form_name,$form_size,$form_tooltip="");', TObject( ttFunc ));
     AddObject('form_hidden($form_name,$form_value);', TObject( ttFunc ));
     AddObject('form_open($form_method,$form_action,$form_name="",$form_target="",$form_enctype="");', TObject( ttFunc ));
     AddObject('form_option($form_method,$form_action,$form_name="",$form_target="",$form_enctype="");', TObject( ttFunc ));
     AddObject('form_password($form_name,$form_size,$form_value,$form_maxlength,$form_class="tbox",$form_readonly="",$form_tooltip="");', TObject( ttFunc ));
     AddObject('form_radio($form_name,$form_value,$form_checked=0,$form_tooltip="");', TObject( ttFunc ));
     AddObject('form_select_close();', TObject( ttFunc ));
     AddObject('form_select_open($form_name);', TObject( ttFunc ));
     AddObject('form_text($form_name,$form_size,$form_value,$form_maxlength,$form_class="tbox",$form_readonly="",$form_tooltip="");', TObject( ttFunc ));
     AddObject('form_textarea($form_name,$form_columns,$form_rows,$form_value,$form_js="",$form_style="",$form_wrap="",$form_readonly="",$form_tooltip="");', TObject( ttFunc ));
     AddObject('getip();', TObject( ttFunc ));
     AddObject('getperms($arg,$ap=ADMINPERMS);', TObject( ttFunc ));
     AddObject('gettime();', TObject( ttFunc ));
     AddObject('init_session();', TObject( ttFunc ));
     AddObject('online($page);', TObject( ttProc ));
     AddObject('preview($news_id,$news_title,$news_body,$news_extended,$news_source,$news_url,$cat_id,$allow_comments,$active_start,$active_end,$news_active);', TObject( ttFunc ));
     AddObject('r_emote();', TObject( ttFunc ));
     AddObject('render_comment($row);', TObject( ttFunc ));
     AddObject('render_newsitem($news_id,$news_title,$news_body,$news_extended,$news_source,$news_url,$news_author,$comment_total,$category_id,$datestamp,$allow_comments,$active_start,$active_end,$news_active,$modex="");', TObject( ttFunc ));
     AddObject('render_poll($poll_id,$poll_question,$poll_option,$votes,$mode,$type="menu");', TObject( ttFunc ));
     AddObject('retrieve_cache($query);', TObject( ttFunc ));
     AddObject('save_prefs($table="core");', TObject( ttProc ));
     AddObject('sendemail($send_to,$subject,$message);', TObject( ttProc ));
     AddObject('set($query,$text);', TObject( ttFunc ));
     AddObject('set_cache();', TObject( ttFunc ));
     AddObject('set_error_handler($type,$message,$file,$line,$context);', TObject( ttProc ));
     AddObject('submit_item($news_id,$news_title,$news_body,$news_extended,$news_source,$news_url,$category_id,$allow_comments,$news_start,$news_end,$news_active);', TObject( ttFunc ));
     AddObject('submit_poll($poll_id,$poll_name,$poll_option,$activate,$id=0,$ref="menu");', TObject( ttFunc ));
     AddObject('tablerender($caption,$text,$mode="default");', TObject( ttFunc ));
     AddObject('textparse();', TObject( ttFunc ));
     AddObject('tp($text,$mode="off");', TObject( ttFunc ));
     AddObject('tpa($text,$mode="off");', TObject( ttFunc ));
     AddObject('tpj();', TObject( ttFunc ));
     AddObject('update_cache($table);', TObject( ttProc ));
     AddObject('userlogin($username,$userpass,$autologin);', TObject( ttFunc ));

{    AddObject('Avatar($user_image);',TObject( ttProc ));
     AddObject('ban();',TObject( ttProc ));
     AddObject('cache_fname($query);', TObject( ttFunc ));
     AddObject('checkvalidtheme($theme_to_check);', TObject( ttFunc ));
     AddObject('check_class($var, $userclass=USERCLASS);',TObject( ttProc ));
     AddObject('clear();', TObject( ttFunc ));
     AddObject('convert_date($datestamp, $mode="long");',TObject( ttFunc ));
     AddObject('dbError($Form);',TObject( ttFunc ));
     AddObject('db_Close();',TObject( ttProc ));
     AddObject('db_Connect($mySQLserver, $mySQLuser, $mySQLpassword,
$mySQLdefaultdb);',TObject( ttFunc ));
     AddObject('db_Count($table, $fields="(*)", $arg="");',TObject( ttFunc ));
     AddObject('db_Delete($table, $arg="");',TObject( ttProc ));
     AddObject('db_Fetch();',TObject( ttFunc ));
     AddObject('db_Fieldname($offset);',TObject( ttFunc ));
     AddObject('db_Insert($table, $arg);',TObject( ttProc ));
     AddObject('db_Rows();',TObject( ttFunc ));
     AddObject('db_Select($table, $fields="*", $arg="",
$mode="default");',TObject( ttFunc ));
     AddObject('db_Select_gen($arg);',TObject( ttFunc ));
     AddObject('db_Update($table, $arg);',TObject( ttProc ));
     AddObject('e107cache_page_md5();', TObject( ttFunc ));
     AddObject('e107_parse($text,$referrer);', TObject( ttFunc ));
     AddObject('editparse($text, $mode="off");', TObject( ttFunc ));
     AddObject('delete($dir, $pattern);', TObject( ttFunc ));
     AddObject('delete_item($news_id);',TObject( ttProc ));
     AddObject('delete_poll($existing);',TObject( ttProc ));
     AddObject('editparse($text, $mode="off");',TObject( ttFunc ));
     AddObject('edit_item($existing);',TObject( ttProc ));
     AddObject('enter_comment($author_name, $comment, $table,
$id);',TObject( ttProc ));
     AddObject('form_button($form_type, $form_name, $form_value,
$form_js="", $form_image="", $form_tooltip="");',TObject( ttProc ));
     AddObject('form_checkbox($form_name, $form_value, $form_checked=0,
$form_tooltip="");',TObject( ttProc ));
     AddObject('form_comment();', TObject( ttFunc ));
     AddObject('form_file($form_name, $form_size, $form_tooltip="");',
TObject( ttFunc ));
     AddObject('form_hidden($form_name, $form_value);', TObject( ttFunc ));
     AddObject('form_open($form_method, $form_action, $form_name="",
$form_target = "", $form_enctype="");', TObject( ttFunc ));
     AddObject('form_option($form_method, $form_action, $form_name="",
$form_target = "", $form_enctype="");', TObject( ttFunc ));
     AddObject('form_password($form_name, $form_size, $form_value,
$form_maxlength, $form_class="tbox", $form_readonly="",
$form_tooltip="");', TObject( ttFunc ));
     AddObject('form_radio($form_name, $form_value, $form_checked=0,
$form_tooltip="");', TObject( ttFunc ));
     AddObject('form_select_close();', TObject( ttFunc ));
     AddObject('form_select_open($form_name);', TObject( ttFunc ));
     AddObject('form_text($form_name, $form_size, $form_value,
$form_maxlength, $form_class="tbox", $form_readonly="",
$form_tooltip="");', TObject( ttFunc ));
     AddObject('form_textarea($form_name, $form_columns, $form_rows,
$form_value, $form_js="", $form_style="", $form_wrap="", $form_readonly="",
$form_tooltip="");', TObject( ttFunc ));
     AddObject('flood($table, $orderfield);',TObject( ttFunc ));
     AddObject('getip();', TObject( ttFunc ));
     AddObject('getperms($arg, $ap = ADMINPERMS);', TObject( ttFunc ));
     AddObject('gettime();', TObject( ttFunc ));
     AddObject('init_session();', TObject( ttFunc ));
     AddObject('online($page);', TObject( ttProc ));
     AddObject('preview($news_id, $news_title, $news_body, $news_extended,
$news_source, $news_url, $cat_id, $allow_comments, $active_start,
$active_end, $news_active);', TObject( ttFunc ));
     AddObject('render_comment($row);', TObject( ttFunc ));
     AddObject('render_newsitem($news_id, $news_title, $news_body,
$news_extended, $news_source, $news_url, $news_author, $comment_total,
$category_id, $datestamp, $allow_comments, $active_start, $active_end,
$news_active, $modex="");', TObject( ttFunc ));
     AddObject('render_poll($poll_id, $poll_question, $poll_option, $votes,
$mode, $type="menu");', TObject( ttFunc ));
     AddObject('r_emote();', TObject( ttFunc ));
     AddObject('save_prefs($table = "core");', TObject( ttProc ));
     AddObject('sendemail($send_to, $subject, $message);', TObject( ttProc ));
     AddObject('set($query, $text);', TObject( ttFunc ));
     AddObject('set_cache();', TObject( ttFunc ));
     AddObject('set_error_handler($type, $message, $file, $line,
$context);', TObject( ttProc ));
     AddObject('submit_item($news_id, $news_title, $news_body,
$news_extended, $news_source, $news_url, $category_id, $allow_comments,
$news_start, $news_end, $news_active);', TObject( ttFunc ));
     AddObject('submit_poll($poll_id, $poll_name, $poll_option, $activate,
$id=0, $ref="menu");', TObject( ttFunc ));
     AddObject('tablerender($caption, $text, $mode="default");', TObject(
ttFunc ));
     AddObject('tp($text, $mode="off");', TObject( ttFunc ));
     AddObject('tpa($text, $mode="off");', TObject( ttFunc ));
     AddObject('tpj();', TObject( ttFunc ));
     AddObject('update_cache($table);', TObject( ttProc ));
     AddObject('userlogin($username, $userpass, $autologin);', TObject(
ttFunc ));
     AddObject('retrieve_cache($query);', TObject( ttFunc ));
     AddObject('textparse();', TObject( ttFunc ));}
   end;
end;(* create *)

end.



