{***************************************************************************}
{ TMS Spell Check component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit TMSSpellCheckReg;

{$I TMSDEFS.INC}

{$IFNDEF FMXLIB}
{$R TMSSpellCheckReg.dcr}
{$ENDIF}

{$IFDEF FMXLIB}
{$R FMX.TMSSpellCheckReg.dcr}
{$ENDIF}

interface

uses
  Classes, TMSSpellCheck, TMSSpellCheckCorrectForm,
  TMSSpellCheckCorrectLinesForm, TMSSpellCheckConfForm
  {$IFDEF FMXLIB}
  , TMSSpellCheckLang
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS SpellCheck', [TAdvSpellCheck
    , TAdvSpellCheckCorrectDialog
    , TAdvSpellCheckCorrectPanel
    , TAdvSpellCheckLanguageSelectDialog
    , TAdvSpellCheckConfigDialog
    , TAdvSpellCheckCorrectLinesPanel
    , TAdvSpellCheckCorrectLinesDialog
    ]);
end;

end.
