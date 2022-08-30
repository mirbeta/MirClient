{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSfmAutoHFTextFmt;

interface

{$I cxVer.inc}

uses
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  ToolWin, Menus, dxCore, dxPSForm, dxPgsDlg, cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxEdit,
  cxLabel, cxGroupBox, cxGraphics, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxListBox, cxGeometry, cxLookAndFeels,
  dxPSHFToolBarBld, dxLayoutcxEditAdapters, dxLayoutContainer, dxLayoutControlAdapters, dxLayoutLookAndFeels,
  cxClasses, dxLayoutControl;

type
  TdxfmAutoText = class(TCustomdxPSForm)
    btnAdd: TcxButton;
    btnCancel: TcxButton;
    btnDelete: TcxButton;
    btnHelp: TcxButton;
    btnOK: TcxButton;
    cbxAutoEntries: TcxTextEdit;
    cbxAutoEntriesList: TcxListBox;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutGroup7: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    gbxEnterAutoTextEntriesHere: TdxLayoutGroup;
    lblPreview: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    libtnHelp: TdxLayoutItem;
    pbxPreview: TPaintBox;
    ToolBar: TPanel;

    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure cbxAutoEntriesEnter(Sender: TObject);
    procedure cbxAutoEntriesExit(Sender: TObject);
    procedure cbxAutoEntriesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbxAutoEntriesListClick(Sender: TObject);
    procedure cbxAutoEntriesListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbxAutoEntriesPropertiesChange(Sender: TObject);
    procedure NewAutoTextChange(Sender: TObject);
    procedure NewAutoTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pbxPreviewPaint(Sender: TObject);
  private
    FFunctionList: TStringList;
    FSelectedEntryIndex: Integer;
    FToolBarHelper: TdxPSHFToolBarHelper;
    FToolBarImages: TcxImageList;

    function GetEntries: TStrings;
    procedure DoInsertMacrosValue(Sender: TObject; const AMacrosValue: string);
    procedure EnableToolbar(Value: Boolean);
    procedure LoadTranslations;
    procedure SetEntries(Value: TStrings);
    procedure SetSelectedEntryIndex(AIndex: Integer);
    procedure UpdateControlsState;
    //
    property FunctionList: TStringList read FFunctionList;
    property ToolBarImages: TcxImageList read FToolBarImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;

    property Entries: TStrings read GetEntries write SetEntries;
    property SelectedEntryIndex: Integer read FSelectedEntryIndex write SetSelectedEntryIndex;
  end;

function dxShowAutoTextDlg(AStrings: TStrings): Boolean;

implementation

{$R *.DFM}

uses
  Registry, Math, dxPSCore, dxPSRes, dxPSUtl, dxPsGlbl, dxPSESys, dxPSEvnt, dxPSHFLibrary, dxDPIAwareUtils;

function dxShowAutoTextDlg(AStrings: TStrings): Boolean;
var
  Dialog: TdxfmAutoText;
begin
  Result := False;
  if AStrings = nil then Exit;

  Dialog := TdxfmAutoText.Create(nil);
  with Dialog do
  try
    Entries := AStrings;
    Result := Execute;
    if Result then
      AStrings.Assign(Entries);
  finally
    Free;
  end;
end;

{ TdxfmAutoText }

constructor TdxfmAutoText.Create(AOwner: TComponent);
begin
  HelpContext := dxhcAutoHFTextEntriesDlg;
  inherited Create(AOwner);
  CheckDialogFormHelpContext(Self, libtnHelp);

  FFunctionList := TStringList.Create;
  dxGetHFFunctionsList(FFunctionList);

  FToolBarImages := TcxImageList.Create(Self);
  FToolBarImages.SourceDPI := dxDefaultDPI;
  FToolBarImages.AllocBy := FFunctionList.Count;

  FToolBarHelper := TdxPSHFToolBarHelper.Create(ToolBar);
  FToolBarHelper.ButtonsImageList := ToolBarImages;
  FToolBarHelper.OnInsertMacros := DoInsertMacrosValue;
  FToolBarHelper.Build(FunctionList, nil, nil, False);
  LoadTranslations;
end;

destructor TdxfmAutoText.Destroy;
begin
  FreeAndNil(FFunctionList);
  FreeAndNil(FToolBarHelper);
  inherited Destroy;
end;

function TdxfmAutoText.Execute: Boolean;
begin
  UpdateControlsState;
  Result := ShowModal = mrOK;
end;

procedure TdxfmAutoText.pbxPreviewPaint(Sender: TObject);

  procedure DrawPart(ACanvas: TcxCanvas; S: string; ARect: TRect; AFormat: LongInt);
  begin
    S := dxProcessHFString(S);
    if S <> '' then
      ACanvas.DrawTexT(S, ARect, cxAlignTop or AFormat);
  end;

  procedure DrawEntryText(ACanvas: TcxCanvas; const S: string; const R: TRect);
  var
    APart1, APart2, APart3: string;
    ARect: TRect;
    AThirdSize: Integer;
  begin
    ARect := R;
    AThirdSize := cxRectWidth(ARect) div 3;
    dxPSSplitAutoHFTextEntry(S, APart1, APart2, APart3);
    DrawPart(ACanvas, APart1, ARect, cxAlignLeft);
    Inc(ARect.Left, AThirdSize);
    Dec(ARect.Right, AThirdSize);
    DrawPart(ACanvas, APart2, ARect, cxAlignHCenter);
    OffsetRect(ARect, AThirdSize, 0);
    DrawPart(ACanvas, APart3, ARect, cxAlignRight);
  end;

var
  ABounds: TRect;
  ACanvas: TcxCanvas;
begin
  ABounds := TPaintBox(Sender).ClientRect;
  ACanvas := TcxCanvas.Create(TPaintBox(Sender).Canvas);
  try
    ACanvas.FrameRect(ABounds, Painter.DefaultGroupTextColor);
    InflateRect(ABounds, -1, -1);
    ACanvas.FillRect(ABounds, clWindow);
    ACanvas.Brush.Style := bsClear;
    if SelectedEntryIndex > -1 then
    begin
      InflateRect(ABounds, -2, -2);
      DrawEntryText(ACanvas, Entries[SelectedEntryIndex], ABounds);
    end;
  finally
    ACanvas.Free;
  end;
end;

procedure TdxfmAutoText.NewAutoTextChange(Sender: TObject);
begin
  UpdateControlsState;
end;

procedure TdxfmAutoText.NewAutoTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      ModalResult := mrCancel;
    VK_RETURN:
      begin
        btnAdd.Click;
        Key := 0;
      end;
  end;
end;

procedure TdxfmAutoText.btnAddClick(Sender: TObject);
begin
  Entries.Add(cbxAutoEntries.Text);
  cbxAutoEntries.Text := '';
  UpdateControlsState;
  ActiveControl := cbxAutoEntries;
end;

procedure TdxfmAutoText.btnDeleteClick(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex := SelectedEntryIndex;
  if AIndex > -1 then
  begin
    cbxAutoEntriesList.Items.Delete(AIndex);
    AIndex := Min(AIndex, cbxAutoEntriesList.Items.Count - 1);
    SelectedEntryIndex := AIndex;
    UpdateControlsState;
  end;
end;

procedure TdxfmAutoText.cbxAutoEntriesEnter(Sender: TObject);
begin
  EnableToolbar(True);
end;

procedure TdxfmAutoText.cbxAutoEntriesExit(Sender: TObject);
begin
  EnableToolbar(False);
end;

procedure TdxfmAutoText.cbxAutoEntriesPropertiesChange(Sender: TObject);
begin
  SelectedEntryIndex := Entries.IndexOf(cbxAutoEntries.Text);
  UpdateControlsState;
end;

procedure TdxfmAutoText.cbxAutoEntriesListClick(Sender: TObject);
begin
  SelectedEntryIndex := cbxAutoEntriesList.ItemIndex;
end;

procedure TdxfmAutoText.DoInsertMacrosValue(Sender: TObject; const AMacrosValue: string);
begin
  if cbxAutoEntries.Focused then
    cbxAutoEntries.SelText := AMacrosValue;
end;

function TdxfmAutoText.GetEntries: TStrings;
begin
  Result := cbxAutoEntriesList.Items;
end;

procedure TdxfmAutoText.SetEntries(Value: TStrings);
begin
  cbxAutoEntriesList.Items := Value;
end;

procedure TdxfmAutoText.SetSelectedEntryIndex(AIndex: Integer);
begin
  FSelectedEntryIndex := AIndex;
  cbxAutoEntriesList.ItemIndex := AIndex;
  if not cbxAutoEntries.Focused then
  begin
    if AIndex < 0 then
      cbxAutoEntries.Text := ''
    else
      cbxAutoEntries.Text := Entries[AIndex];
  end;
  UpdateControlsState;
  pbxPreview.Invalidate;
end;

procedure TdxfmAutoText.EnableToolbar(Value: Boolean);
var
  I: Integer;
begin
  ToolBar.Enabled := Value;
  for I := 0 to ToolBar.ControlCount - 1 do
    ToolBar.Controls[I].Enabled := Value;
end;

procedure TdxfmAutoText.LoadTranslations;
begin
  Caption := cxGetResourceString(@sdxAutoTextDialogCaption);
  btnOK.Caption := cxGetResourceString(@sdxBtnOK);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);
  btnAdd.Caption := cxGetResourceString(@sdxBtnAdd);
  btnDelete.Caption := cxGetResourceString(@sdxBtnDelete);
  gbxEnterAutoTextEntriesHere.Caption := cxGetResourceString(@sdxEnterAutoTextEntriesHere);
  lblPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));
end;

procedure TdxfmAutoText.UpdateControlsState;
var
  AIndex: Integer;
begin
  AIndex := Entries.IndexOf(cbxAutoEntries.Text);
  btnDelete.Enabled := AIndex > -1;
  btnAdd.Enabled := (AIndex = -1) and (cbxAutoEntries.Text <> '');
end;

procedure TdxfmAutoText.cbxAutoEntriesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_HOME:
      SelectedEntryIndex := 0;
    VK_END:
      SelectedEntryIndex := cbxAutoEntriesList.Count - 1;
    VK_UP:
      SelectedEntryIndex := Max(SelectedEntryIndex - 1, 0);
    VK_DOWN:
      SelectedEntryIndex := Min(SelectedEntryIndex + 1, cbxAutoEntriesList.Count - 1);
  end;
end;

procedure TdxfmAutoText.cbxAutoEntriesListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  SelectedEntryIndex := cbxAutoEntriesList.ItemIndex;
end;

end.

