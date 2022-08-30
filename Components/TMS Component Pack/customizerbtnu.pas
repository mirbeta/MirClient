{*************************************************************************}
{ TMS ToolBars component                                                  }
{ for Delphi & C++Builder                                                 }
{ version 1.4                                                             }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2006                                              }
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

unit CustomizerBtnU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, AdvToolBar;

type
  TCusBtnForm = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    GroupBox1: TGroupBox;
    LblBtnCaption: TLabel;
    LblBtnHint: TLabel;
    LblGlyph: TLabel;
    ChkBtnVis: TCheckBox;
    ChkBtnShowCap: TCheckBox;
    EdtBtnHint: TEdit;
    EdtBtnCaption: TEdit;
    CmBxGlyph: TComboBox;
    procedure OkBitBtnClick(Sender: TObject);
    procedure CmBxGlyphDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
    FToolBarCustomizer: TAdvToolBarCustomizer;
  end;

var
  CusBtnForm: TCusBtnForm;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TCusBtnForm.OkBitBtnClick(Sender: TObject);
begin

  ModalResult := mrOk;
end;

//------------------------------------------------------------------------------

procedure TCusBtnForm.CmBxGlyphDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  CmBxGlyph.Canvas.FillRect(Rect);

  if Assigned(FToolBarCustomizer) and Assigned(FToolBarCustomizer.AdvToolBar) and Assigned(FToolBarCustomizer.AdvToolBar.Images) and (Index < FToolBarCustomizer.AdvToolBar.Images.Count) then
    FToolBarCustomizer.AdvToolBar.Images.Draw(CmBxGlyph.Canvas, Rect.Left + 2, Rect.Top + 2, Index)
  else if Assigned(FToolBarCustomizer) and Assigned(FToolBarCustomizer.AdvToolBar) and Assigned(FToolBarCustomizer.AdvToolBar.Images) and (Index = FToolBarCustomizer.AdvToolBar.Images.Count) then
  begin
    CmBxGlyph.Canvas.TextOut(Rect.Left + 4, Rect.Top+((CmBxGlyph.ItemHeight- CmBxGlyph.Canvas.TextHeight('X')) div 2), CmBxGlyph.Items[Index]);
  end;

end;

//------------------------------------------------------------------------------

end.
