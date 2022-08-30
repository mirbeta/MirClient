{*************************************************************************}
{ TMS TAdvRichEditor                                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}
unit AdvRichEditorReg;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvRichEditor, AdvRichEditorBase, AdvRichEditorToolBar,
  AdvRichEditorIO, AdvRichEditorPDFIO, AdvRichEditorMiniHTMLIO
  {$IFDEF TMSPACK}
  , AdvRichEditorEmoticons, AdvRichEditorPopupToolBar
  {$ENDIF}
  ;

{$R AdvRichEditorReg.dcr}

procedure Register;

implementation

uses
  ActnList
  {$IFDEF DELPHIXE3_LVL}
  , System.Actions
  {$ENDIF}
  ;

procedure Register;
begin
  RegisterComponents('TMS Edits',[TAdvRichEditor]);
  RegisterComponents('TMS Edits',[TAdvRichEditorHTMLIO, TAdvRichEditorRTFIO]);

  {$IFDEF TMSPACK}
  RegisterComponents('TMS Edits',[TAdvRichEditorSmallEmoticons, TAdvRichEditorLargeEmoticons]);
  RegisterComponents('TMS Edits',[TAdvRichEditorPopupToolBar]);
  {$ENDIF}

  RegisterComponents('TMS Edits',[TAdvRichEditorFormatToolBar,
                                  TAdvRichEditorEditingToolBar,
                                  TAdvRichEditorEditToolBar]);

  RegisterComponents('TMS Edits',[TAdvRichEditorClipboardRibbonToolBar,
                                  TAdvRichEditorFontRibbonToolBar,
                                  TAdvRichEditorParagraphRibbonToolBar,
                                  TAdvRichEditorInsertRibbonToolBar,
                                  TAdvRichEditorEditingRibbonToolBar
                                  ]);

  RegisterComponents('TMS Edits', [TAdvRichEditorPDFIO]);

  RegisterComponents('TMS Edits', [TAdvRichEditorMiniHTMLIO]);

  RegisterActions('AdvRichEdit',[
    TAdvRichEditorClear,
    TAdvRichEditorCut,
    TAdvRichEditorCopy,
    TAdvRichEditorPaste,
    TAdvRichEditorSelectAll,
    TAdvRichEditorAlignRight,
    TAdvRichEditorAlignCenter,
    TAdvRichEditorAlignLeft,
    TAdvRichEditorBold,
    TAdvRichEditorItalic,
    TAdvRichEditorUnderline,
    TAdvRichEditorStrikeOut,
    TAdvRichEditorSubScript,
    TAdvRichEditorSuperScript,
    TAdvRichEditorTextColor,
    TAdvRichEditorFontName,
    TAdvRichEditorFontSize,
    TAdvRichEditorBulletType,
    TAdvRichEditorNumberedBulletType,
    TAdvRichEditorColor,
    TAdvRichEditorIndent,
    TAdvRichEditorUnIndent,
    TAdvRichEditorUndo,
    TAdvRichEditorRedo
    ],nil);
end;

end.
