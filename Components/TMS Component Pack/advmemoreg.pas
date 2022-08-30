{***************************************************************************}
{ TAdvMemo component                                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2015                                        }
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

unit AdvMemoReg;

interface

{$I TMSDEFS.INC}

uses
  Classes,
  AdvMemo,
  AdvmCSS,
  AdvmPS,
  AdvmBS,
  AdvmWS,
  AdvmSQLS,
  AdvmCSHS,
  AdvmPYS,
  AdvmPLS,
  AdvmES,
  AdvmINIS,
  AdvmXML,
  AdvmPHP,
  AdvmLua,
  AdvmJSON,
  AdvCodeList,
  AdvMemoStylerManager,
  DBAdvMemo
  {$IFDEF DELPHIXE_LVL}
  , AdvMemoPDFIO
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.Actions
  {$ENDIF}
  ;

procedure Register;

implementation

uses
  ActnList;                                            

procedure Register;
begin
  RegisterComponents('TMS Memo', [TAdvMemo,TAdvMemoSource,
                                  TDBAdvMemo,
                                  TAdvHTMLMemoStyler,
                                  TAdvJSMemoStyler,
                                  TAdvWebMemoStyler,
                                  TAdvPascalMemoStyler,
                                  TAdvBasicMemoStyler,
                                  TAdvCSSMemoStyler,
                                  TAdvCSharpMemoStyler,
                                  TAdvSQLMemoStyler,
                                  TAdvPythonMemoStyler,
                                  TAdvPerlMemoStyler,
                                  TAdvLuaMemoStyler,
                                  TAdvEmoticonMemoStyler,
                                  TAdvMemoFindDialog,
                                  TAdvMemoFindReplaceDialog,
                                  TAdvCodeList,
                                  TAdvMemoStylerManager,
                                  TAdvINIMemoStyler,
                                  TAdvXMLMemoStyler,
                                  TAdvJSONMemoStyler,
                                  TAdvMemoCapitalChecker,
                                  TAdvPHPMemoStyler]);

  {$IFDEF DELPHIXE_LVL}
  RegisterComponents('TMS Memo', [TAdvMemoPDFIO]);
  {$ENDIF}

  RegisterActions('AdvMemo', [TAdvMemoCut, TAdvMemoCopy, TAdvMemoPaste,
    TAdvMemoDelete, TAdvMemoUndo, TAdvMemoRedo, TAdvMemoSelectAll], nil);
end;

end.
