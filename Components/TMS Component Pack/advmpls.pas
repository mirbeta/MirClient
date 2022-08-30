{***************************************************************************}
{ TAdvMemo styler component                                                 }
{ for Delphi & C++Builder                                                   }
{ version 1.5                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2002 - 2004                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}
{$I TMSDEFS.INC}
unit AdvmPLS;

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
  TAdvPerlMemoStyler=class(TAdvCustomMemoStyler)
  private
    FVersion: string;
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

    property Version: string read FVersion;
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
  end;


implementation

{ TAdvPascalMemoStyler }
constructor TAdvPerlMemoStyler.Create(AOwner: TComponent);
var
  itm:TElementStyle;
begin
  inherited;
  FVersion := '1.0';
  Description := 'Perl';
  Filter := 'Perl Files (*.pl,*.pm,*.cgi)|*.pl;*.pm;*.cgi';
  DefaultExtension := '.pl';
  StylerName := 'Perl';
  Extensions := 'pl;pm;cgi';

  LineComment := '#';
  MultiCommentLeft := '"""';
  MultiCommentRight := '"""';
  CommentStyle.TextColor := clNavy;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clFuchsia;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];
  BlockStart := '';
  BlockEnd := '';
  HexIdentifier := '';
  //------------Pascal Standard Default-------------
  itm := AllStyles.Add;
  itm.Info := 'Perl keywords';
  itm.Font.Color := clGreen;
  itm.Font.Style := [fsBold];

  with itm.KeyWords do
  begin
    Add('AND');
    Add('CMP');
    Add('CONTINUE');
    Add('DO');
    Add('ELSE');
    Add('ELSIF');
    Add('EQ');
    Add('FOR');
    Add('FOREACH');
    Add('GE');
    Add('GOTO');
    Add('GT');
    Add('IF');
    Add('LAST');
    Add('LE');
    Add('LT');
    Add('MY');
    Add('NE');
    Add('NEXT');
    Add('NOT');
    Add('OR');
    Add('PACKAGE');
    Add('RETURN');
    Add('SUB');
    Add('SWITCH');
    Add('UNLESS');
    Add('UNTIL');
    Add('USE');
    Add('WHILE');
    Add('XOR');
    Add('STDIN');
    Add('STDOUT');
    Add('STDERR');
    Add('ARGV');
    Add('ARGVOUT');
    Add('ENV');
    Add('INC');
    Add('SIG');
    Add('TRUE');
    Add('FALSE');
    Add('__FILE__');
    Add('__LINE__');
    Add('__PACKAGE__');
    Add('__END__');
    Add('__DATA__');

  end;

  itm := AllStyles.Add;
  itm.Info := 'Perl functions';
  itm.Font.Color := clGreen;
  itm.Font.Style := [fsBold];
  with itm.KeyWords do
  begin
     Add('abs');
     Add('accept');
     Add('alarm');
     Add('atan2');
     Add('bind');
     Add('binmode');
     Add('bless');
     Add('caller');
     Add('chdir');
     Add('chmod');
     Add('chomp');
     Add('chop');
     Add('chown');
     Add('chr');
     Add('chroot');
     Add('close');
     Add('closedir');
     Add('connect');
     Add('cos');
     Add('crypt');
     Add('dbmclose');
     Add('dbmopen');
     Add('defined');
     Add('delete');
     Add('die');
     Add('dump');
     Add('each');
     Add('eof');
     Add('eval');
     Add('exec');
     Add('exists');
     Add('exit');
     Add('exp');
     Add('fcntl');
     Add('fileno');
     Add('flock');
     Add('fork');
     Add('format');
     Add('formline');
     Add('getc');
     Add('getlogin');
     Add('getpeername');
     Add('getpgrp');
     Add('getppid');
     Add('getpriority');
     Add('getpwnam');
     Add('getgrnam');
     Add('gethostbyname');
     Add('getnetbyname');
     Add('getprotobyname');
     Add('getpwuid');
     Add('getgrgid');
     Add('getservbyname');
     Add('gethostbyaddr');
     Add('getnetbyaddr');
     Add('getprotobynumber');
     Add('getservbyport');
     Add('getpwent');
     Add('getgrent');
     Add('gethostent');
     Add('getnetent');
     Add('getprotoent');
     Add('getservent');
     Add('setpwent');
     Add('setgrent');
     Add('sethostent');
     Add('setnetent');
     Add('setprotoent');
     Add('setservent');
     Add('endpwent');
     Add('endgrent');
     Add('endhostent');
     Add('endnetent');
     Add('endprotoent');
     Add('endservent');
     Add('getsockname');
     Add('getsockopt');
     Add('glob');
     Add('gmtime');
     Add('grep');
     Add('hex');
     Add('import');
     Add('index');
     Add('int');
     Add('ioctl');
     Add('join');
     Add('keys');
     Add('kill');
     Add('lc');
     Add('lcfirst');
     Add('length');
     Add('link');
     Add('listen');
     Add('localtime');
     Add('log');
     Add('lstat');
     Add('mkdir');
     Add('msgctl');
     Add('msgget');
     Add('msgsnd');
     Add('msgrcv');
     Add('no');
     Add('oct');
     Add('open');
     Add('opendir');
     Add('ord');
     Add('pack');
     Add('pipe');
     Add('pop');
     Add('pos');
     Add('print');
     Add('printf');
     Add('prototype');
     Add('push');
     Add('quotemeta');
     Add('rand');
     Add('read');
     Add('readdir');
     Add('readlink');
     Add('ref');
     Add('rename');
     Add('reset');
     Add('reverse');
     Add('rewinddir');
     Add('rindex');
     Add('rmdir');
     Add('scalar');
     Add('seek');
     Add('seekdir');
     Add('select');
     Add('semctl');
     Add('semget');
     Add('semop');
     Add('send');
     Add('setpgrp');
     Add('setpriority');
     Add('setsockopt');
     Add('shift');
     Add('shmctl');
     Add('shmget');
     Add('shmread');
     Add('shmwrite');
     Add('shutdown');
     Add('sin');
     Add('sleep');
     Add('socket');
     Add('socketpair');
     Add('sort');
     Add('splice');
     Add('split');
     Add('sprintf');
     Add('sqrt');
     Add('srand');
     Add('stat');
     Add('study');
     Add('substr');
     Add('symlink');
     Add('syscall');
     Add('sysopen');
     Add('sysread');
     Add('sysseek');
     Add('system');
     Add('syswrite');
     Add('tell');
     Add('telldir');
     Add('tie');
     Add('tied');
     Add('time');
     Add('times');
     Add('truncate');
     Add('uc');
     Add('ucfirst');
     Add('umask');
     Add('undef');
     Add('unlink');
     Add('unpack');
     Add('untie');
     Add('unshift');
     Add('utime');
     Add('values');
     Add('vec');
     Add('wait');
     Add('waitpid');
     Add('wantarray');
     Add('warn');
     Add('write');
   end;

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
  itm.Symbols := #32+',(){}[]-+*/=~!&|<>?:;.'+#13+#10;

end;


end.



