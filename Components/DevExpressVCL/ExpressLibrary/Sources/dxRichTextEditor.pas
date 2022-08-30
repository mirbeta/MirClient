{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxRichTextEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ToolWin, ImgList, cxGraphics, StdActns,
  ExtActns, ActnList, Menus;

type
  TRichEdit = class(ComCtrls.TRichEdit)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TfrmRichTextEditor = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    edtRich: TRichEdit;
    FontDialog1: TFontDialog;
    ToolBar1: TToolBar;
    tlbOpen: TToolButton;
    tlbSave: TToolButton;
    ToolButton1: TToolButton;
    tlbCopy: TToolButton;
    tlbPaste: TToolButton;
    tlbCut: TToolButton;
    ToolButton2: TToolButton;
    ActionList1: TActionList;
    RichEditBold1: TRichEditBold;
    RichEditItalic1: TRichEditItalic;
    RichEditUnderline1: TRichEditUnderline;
    RichEditStrikeOut1: TRichEditStrikeOut;
    RichEditBullets1: TRichEditBullets;
    RichEditAlignLeft1: TRichEditAlignLeft;
    RichEditAlignRight1: TRichEditAlignRight;
    RichEditAlignCenter1: TRichEditAlignCenter;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    PopupMenu1: TPopupMenu;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    cxImageList1: TcxImageList;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolBar2: TToolBar;
    ToolButton7: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    ToolButton16: TToolButton;
    ilToolBar: TcxImageList;
    ilToolBarDisabled: TcxImageList;
    ToolButton5: TToolButton;
    ToolButton17: TToolButton;
    ColorDialog1: TColorDialog;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    procedure btnApplyClick(Sender: TObject);
    procedure edtRichChange(Sender: TObject);
    procedure tlbFontClick(Sender: TObject);
    procedure edtRichSelectionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToolButton17Click(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FOnApply: TNotifyEvent;
    function GetText: string;
    procedure SetChanged(AValue: Boolean);
    procedure SetText(const Value: string);
    function GetModified: Boolean;
  public
    function Execute: Boolean;
    property Text: string read GetText write SetText;
    property Modified: Boolean read GetModified;
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
  end;

implementation

uses
  dxDrawRichTextUtils;

{$R *.dfm}

type
  TStringsArray = array of string;

var
  FRichEditLibrary: HMODULE = 0;

{ TRichEdit }

procedure TRichEdit.CreateParams(var Params: TCreateParams);

  procedure InitRichEditClassNames(var ARichEditClassNames: TStringsArray);
  const
    RichEditClassNamesCount = 5;
  begin
    SetLength(ARichEditClassNames, RichEditClassNamesCount);
    ARichEditClassNames[0] := 'RICHEDIT';
    ARichEditClassNames[1] := 'RICHEDIT20';
    ARichEditClassNames[2] := 'RICHEDIT30';
    ARichEditClassNames[3] := 'RICHEDIT41';
    ARichEditClassNames[4] := 'RICHEDIT50';
  end;

var
  ARichClassName: string;
  AWndClass: TWndClass;
  I: Integer;
  ARichEditClassNames: TStringsArray;
begin
  inherited;

  if FRichEditLibrary <> 0 then
  begin
    InitRichEditClassNames(ARichEditClassNames);
    for I := High(ARichEditClassNames) downto Low(ARichEditClassNames) do
    begin
      ARichClassName := ARichEditClassNames[I] + 'A';
      if GetClassInfo(HInstance, PChar(ARichClassName), AWndClass) then
        Break;
      ARichClassName := ARichEditClassNames[I];
      if GetClassInfo(HInstance, PChar(ARichClassName), AWndClass) then
        Break;
    end;
    if GetClassInfo(HInstance, PChar(ARichClassName), AWndClass) then
      CreateSubClass(Params, PChar(ARichClassName));
  end;
end;

procedure LoadRichDll;

  procedure InitRichEditDLLNames(var ARichEditDLLNames: TStringsArray);
  const
    RichEditDLLNamesCount = 3;
  begin
    SetLength(ARichEditDLLNames, RichEditDLLNamesCount);
    ARichEditDLLNames[0] :=  'Riched32.dll';
    ARichEditDLLNames[1] :=  'Riched20.dll';
    ARichEditDLLNames[2] :=  'Msftedit.dll';
  end;

var
  ARichEditDLLNames: TStringsArray;
  I: Integer;
begin
  InitRichEditDLLNames(ARichEditDLLNames);
  for I := High(ARichEditDLLNames) downto Low(ARichEditDLLNames) do
  begin
    FRichEditLibrary := LoadLibrary(PChar(ARichEditDLLNames[I]));
    if FRichEditLibrary <> 0 then
      Break;
  end;
end;

procedure TfrmRichTextEditor.btnApplyClick(Sender: TObject);
begin
  if Assigned(FOnApply) then
    FOnApply(Self);
  SetChanged(False);
end;

function TfrmRichTextEditor.Execute: Boolean;
begin
  SetChanged(False);
  Result := ShowModal = mrOk;
end;

procedure TfrmRichTextEditor.FileOpen1Accept(Sender: TObject);
begin
  edtRich.Lines.LoadFromFile(FileOpen1.Dialog.FileName);
end;

procedure TfrmRichTextEditor.FileSaveAs1Accept(Sender: TObject);
begin
  edtRich.Lines.SaveToFile(FileSaveAs1.Dialog.FileName);
end;

procedure TfrmRichTextEditor.FormCreate(Sender: TObject);
begin
  ToolBar1.Images := ilToolBar;
  ToolBar1.DisabledImages := ilToolBarDisabled;
  ToolBar2.Images := ilToolBar;
  ToolBar2.DisabledImages := ilToolBarDisabled;
  cxTransformImages(cxImageList1, ilToolBar, clBtnFace);
  cxTransformImages(cxImageList1, ilToolBarDisabled, clBtnFace, False);
  Panel1.Align := alClient;
end;

procedure TfrmRichTextEditor.FormShow(Sender: TObject);
begin
  SetChanged(False);
end;

procedure TfrmRichTextEditor.edtRichChange(Sender: TObject);
begin
  SetChanged(True);
end;

procedure TfrmRichTextEditor.edtRichSelectionChange(Sender: TObject);
begin
//
end;

procedure TfrmRichTextEditor.tlbFontClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(edtRich.SelAttributes);
  if FontDialog1.Execute then
    edtRich.SelAttributes.Assign(FontDialog1.Font);
end;

procedure TfrmRichTextEditor.ToolButton17Click(Sender: TObject);
begin
  ColorDialog1.Color := edtRich.SelAttributes.Color;
  if ColorDialog1.Execute then
    edtRich.SelAttributes.Color := ColorDialog1.Color;
end;

function TfrmRichTextEditor.GetModified: Boolean;
begin
  Result := edtRich.Modified;
end;

function TfrmRichTextEditor.GetText: string;
var
  AStream: TStringStream;
  AEncoding: TEncoding;
begin
  if not edtRich.PlainText then
    AEncoding := TEncoding.Default
  else
    AEncoding := TEncoding.Unicode;

  AStream := TStringStream.Create('', AEncoding);
  try
    edtRich.Lines.SaveToStream(AStream, AEncoding);
    Result := AStream.DataString;
  finally
    AStream.Free;
  end;
end;

procedure TfrmRichTextEditor.SetChanged(AValue: Boolean);
begin
  edtRich.Modified := AValue;
  btnApply.Enabled := edtRich.Modified;
end;

procedure TfrmRichTextEditor.SetText(const Value: string);
begin
//  if edtRich.PlainText then
//    edtRich.Perform(WM_SETTEXT, 0, LPARAM(PChar(Value)))
//  else
    dxRichLoadFromString(edtRich.Lines, Value);
end;

initialization
  LoadRichDll;

finalization
  if FRichEditLibrary <> 0 then
    FreeLibrary(FRichEditLibrary);

end.
