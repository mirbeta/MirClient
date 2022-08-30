{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetEditHyperlinkDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, cxClasses,
  cxPC, dxSpreadSheetCore, dxSpreadSheetStrs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxButtons,
  dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, cxListBox, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, cxLabel, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxSpinEdit, cxCheckBox, Generics.Defaults, Generics.Collections,
  dxSpreadSheetFormatCellsDialog, ExtCtrls, cxFontNameComboBox, cxColorComboBox, cxCheckListBox,
  dxGalleryControl, dxColorGallery, cxImageComboBox, dxColorDialog, ImgList, dxSpreadSheetGraphics, dxCore,
  dxSpreadSheetClasses, dxSpreadSheetTypes, cxImage, cxButtonEdit, ComCtrls, cxTreeView, dxSpreadSheetHyperlinks,
  dxGDIPlusClasses, dxHashUtils, dxSpreadSheetFormulas, cxGeometry, dxForms;

const
  WM_CHANGEFOCUS = WM_USER + 1;

type

  { TdxSpreadSheetFormatCellsDialogForm }

  TdxSpreadSheetEditHyperlinkDialogForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    btnRemoveLink: TcxButton;
    cxImage1: TcxImage;
    cxImage2: TcxImage;
    cxImage3: TcxImage;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    edtAddress: TcxButtonEdit;
    edtCellRef: TcxTextEdit;
    edtEmail: TcxTextEdit;
    edtScreenTip: TcxTextEdit;
    edtSubject: TcxTextEdit;
    edtTextToDisplay: TcxTextEdit;
    lbEMail: TcxLabel;
    lbFileOrWebPage: TcxLabel;
    lbPlaceInDocument: TcxLabel;
    lcbtnCancel: TdxLayoutItem;
    lcbtnOK: TdxLayoutItem;
    lcbtnRemoveLink: TdxLayoutItem;
    lcEditAddress: TdxLayoutItem;
    lcEditCellRef: TdxLayoutItem;
    lcEditEMailAddress: TdxLayoutItem;
    lcEditPlaceInDocument: TdxLayoutItem;
    lcEditScreenTip: TdxLayoutItem;
    lcEditSubject: TdxLayoutItem;
    lcEditTextToDisplay: TdxLayoutItem;
    lcFileOrWebPage: TdxLayoutItem;
    lclbLinkTo: TdxLayoutLabeledItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMainGroup1: TdxLayoutGroup;
    lcMainGroup14: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutAutoCreatedGroup;
    lcMainGroup3: TdxLayoutGroup;
    lcMainItem1: TdxLayoutItem;
    lcMainSeparatorItem1: TdxLayoutSeparatorItem;
    lcMainSpaceItem1: TdxLayoutEmptySpaceItem;
    lcPlaceInDocument: TdxLayoutItem;
    pnEMail: TPanel;
    pnFileOrWebPage: TPanel;
    pnPlaceInDocument: TPanel;
    tvDocumentPlace: TcxTreeView;
    ChooseFile: TFileOpenDialog;

    procedure EditDisplayTextChanged(Sender: TObject);
    procedure EditValueChanged(Sender: TObject);
    procedure edtAddressPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure edtAddressPropertiesChange(Sender: TObject);
    procedure edtEmailExit(Sender: TObject);
    procedure edtEmailPropertiesChange(Sender: TObject);
    procedure edtScreenTipPropertiesChange(Sender: TObject);
    procedure NavbarItemClick(Sender: TObject);
    procedure NavbarItemMouseEnter(Sender: TObject);
    procedure NavbarItemMouseLeave(Sender: TObject);
    procedure tvDocumentPlaceChange(Sender: TObject; Node: TTreeNode);
  strict private
    FActivePage: TdxSpreadSheetHyperlinkValueType;
    FDisplayTextChanged: Boolean;
    FEditingHyperlink: TdxSpreadSheetHyperlink;
    FHottrackPage: Integer;
    FHyperlink: TdxSpreadSheetHyperlink;
    FIsUpdating: Boolean;
    FModified: Boolean;
    FPrevText: string;
    FSheet: TdxSpreadSheetTableView;
    FSheets, FNames: TTreeNode;

    function GetActiveCell: TdxSpreadSheetCell;
    function GetActiveContainer: TdxSpreadSheetContainer;
    function GetArea: TRect;
    function GetDefaultDisplayText: string;
    function GetPageLink(APage: TdxSpreadSheetHyperlinkValueType): TcxCustomEdit;
    function GetReferenceText: string;
    function GetSelectedCell: TdxSpreadSheetCell;
    function GetSelectedNode: TTreeNode;
    function GetSpreadSheet: TdxCustomSpreadSheet;
    function GetValue: string;
    procedure SetActivePage(const AValue: TdxSpreadSheetHyperlinkValueType);
    procedure SetHotTrackPage(AValue: Integer);
    //
    procedure ChangeFocus(var Message: TMessage); message WM_CHANGEFOCUS;
  protected
    procedure ApplyLocalization;
    function CreateEditingHyperlink: TdxSpreadSheetHyperlink; virtual;
    procedure Initialize(ASheet: TdxSpreadSheetTableView; AHyperlink: TdxSpreadSheetHyperlink); virtual;
    procedure InitializeHyperlinkValue;
    procedure PopulateNames;
    procedure SetupControls;
    procedure SetEditFontStyle(AEdit: TcxCustomEdit; AStyles: TFontStyles);
    procedure SynchronizeDisplayText;

    property DisplayTextChanged: Boolean read FDisplayTextChanged write FDisplayTextChanged;
    property EditingHyperlink: TdxSpreadSheetHyperlink read FEditingHyperlink;
    property HottrackPage: Integer read FHottrackPage write SetHotTrackPage;
    property IsUpdating: Boolean read FIsUpdating write FIsUpdating;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load; overload;
    procedure Save; overload;

    property Area: TRect read GetArea;
    property ActiveCell: TdxSpreadSheetCell read GetActiveCell;
    property ActiveContainer: TdxSpreadSheetContainer read GetActiveContainer;
    property ActivePage: TdxSpreadSheetHyperlinkValueType read FActivePage write SetActivePage;
    property Hyperlink: TdxSpreadSheetHyperlink read FHyperlink;
    property Modified: Boolean read FModified write FModified;
    property Sheet: TdxSpreadSheetTableView read FSheet;
    property SelectedCell: TdxSpreadSheetCell read GetSelectedCell;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
  end;

procedure ShowHyperlinkEditorDialog(ASheet: TdxSpreadSheetTableView; AHyperlink: TdxSpreadSheetHyperlink = nil);

implementation

uses
  dxSpreadSheetDialogStrs, dxSpreadSheetUtils, dxSpreadSheetCoreFormulasParser;

{$R *.dfm}

const
  URLPrefix = 'http://';

type
  TdxSpreadSheetHistoryAccess = class(TdxSpreadSheetHistory);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetDefinedNameAccess = class(TdxSpreadSheetDefinedName);
  TdxLayoutContainerAccess = class(TdxLayoutContainer);
  TdxFocusControllerAccess = class(TdxLayoutContainerFocusController);
  TdxSpreadSheetHyperlinksAccess = class(TdxSpreadSheetHyperlinks);
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);

  { TdxSpreadSheetEditingHyperlink }

  TdxSpreadSheetEditingHyperlink = class(TdxSpreadSheetHyperlink)
  strict private
    FOwner: TdxSpreadSheetEditHyperlinkDialogForm;
  protected
    function CreateData(AType: TdxSpreadSheetHyperlinkValueType): TObject; override;
    procedure Changed; override;
    function IsHistoryAvailable: Boolean; override;
    procedure RelatedCellSetDisplayText; override;
    procedure RestoreStyle; override;
    procedure RemoveFromOwner; override;
  public
    constructor Create(AOwner: TdxSpreadSheetEditHyperlinkDialogForm); reintroduce;

    property Owner: TdxSpreadSheetEditHyperlinkDialogForm read FOwner;
  end;

procedure ShowHyperlinkEditorDialog(ASheet: TdxSpreadSheetTableView; AHyperlink: TdxSpreadSheetHyperlink = nil);
var
  ADialogForm: TdxSpreadSheetEditHyperlinkDialogForm;
begin
  ADialogForm := TdxSpreadSheetEditHyperlinkDialogForm.Create(GetParentForm(ASheet.SpreadSheet));
  try
    if ASheet <> nil then
      SetControlLookAndFeel(ADialogForm, ASheet.SpreadSheet.DialogsLookAndFeel);
    ADialogForm.Initialize(ASheet, AHyperlink);
    case ADialogForm.ShowModal of
      mrOk:
        ADialogForm.Save;
      mrAbort:
        ADialogForm.Hyperlink.Free;
    end;
  finally
    ADialogForm.Free;
  end;
end;

{ TdxSpreadSheetEditingHyperlink }

constructor TdxSpreadSheetEditingHyperlink.Create(AOwner: TdxSpreadSheetEditHyperlinkDialogForm);
begin
  FOwner := AOwner;
  inherited Create(AOwner.Sheet.Hyperlinks);
end;

function TdxSpreadSheetEditingHyperlink.CreateData(AType: TdxSpreadSheetHyperlinkValueType): TObject;
begin
  if Owner.Hyperlink <> nil then
    Result := TdxSpreadSheetEditingHyperlink(Owner.Hyperlink).CreateData(AType)
  else
    Result := inherited CreateData(AType);
end;

procedure TdxSpreadSheetEditingHyperlink.Changed;
begin
end;

function TdxSpreadSheetEditingHyperlink.IsHistoryAvailable: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetEditingHyperlink.RelatedCellSetDisplayText;
begin
end;

procedure TdxSpreadSheetEditingHyperlink.RestoreStyle;
begin
end;

procedure TdxSpreadSheetEditingHyperlink.RemoveFromOwner;
begin
end;

{ TdxSpreadSheetFormatCellsDialogForm }

constructor TdxSpreadSheetEditHyperlinkDialogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ApplyLocalization;
end;

destructor TdxSpreadSheetEditHyperlinkDialogForm.Destroy;
begin
  FreeAndNil(FEditingHyperlink);
  inherited Destroy;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.Load;
begin
  IsUpdating := True;
  try
    lcEditTextToDisplay.Visible := ActiveContainer = nil;
    edtTextToDisplay.Enabled := (SelectedCell = ActiveCell) and ((ActiveCell = nil) or
      (ActiveCell.DataType in [cdtBlank, cdtString]) and lcEditTextToDisplay.Visible);

    if edtTextToDisplay.Enabled then
    begin
      if (FHyperlink <> nil) or (ActiveCell = nil) then
        edtTextToDisplay.Text := EditingHyperlink.DisplayText
      else
        edtTextToDisplay.Text := ActiveCell.DisplayText
    end
    else
      edtTextToDisplay.Text := cxGetResourceString(@scxSelectionInDocument);

    edtScreenTip.Text := EditingHyperlink.ScreenTip;
    case EditingHyperlink.ValueType of
      hvtFileOrWebPage:
        edtAddress.Text := EditingHyperlink.Value;

      hvtEMail:
        begin
          edtSubject.Text := TdxSpreadSheetEditingHyperlink(EditingHyperlink).MailSubject;
          edtEmail.Text := dxEMailHyperlinkMailtoPrefix + TdxSpreadSheetEditingHyperlink(EditingHyperlink).MailAddress;
          FPrevText := edtSubject.Text;
        end;

      hvtReference:
        begin
          tvDocumentPlace.Selected := GetSelectedNode;
          if tvDocumentPlace.Selected <> nil then
            tvDocumentPlace.Selected.Focused := True;
          if TdxSpreadSheetEditingHyperlink(EditingHyperlink).ReferenceName = nil then
            edtCellRef.Text := GetReferenceText;
        end;
    end;

    if ((tvDocumentPlace.Selected = nil) or (tvDocumentPlace.Selected.Level = 0)) and (GetSelectedNode <> nil) then
      GetSelectedNode.Selected := True;
  finally
    Modified := False;
    DisplayTextChanged := (edtTextToDisplay.Text <> '') and (edtTextToDisplay.Text <> GetDefaultDisplayText);
    IsUpdating := False;
  end;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.Save;
begin
  TdxSpreadSheetHyperlinksAccess(Sheet.Hyperlinks).AssignChanges(EditingHyperlink, Hyperlink);
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxHyperlinkEditorInsertCaption);
  if Hyperlink <> nil then
    Caption := cxGetResourceString(@sdxHyperlinkEditorEditCaption);

  btnCancel.Caption := cxGetResourceString(@sdxHyperlinkEditorCancel);
  btnOK.Caption := cxGetResourceString(@sdxHyperlinkEditorOK);
  btnRemoveLink.Caption := cxGetResourceString(@sdxHyperlinkEditorRemoveLink);
  lcbtnRemoveLink.Visible := Hyperlink <> nil;

  lclbLinkTo.Caption := cxGetResourceString(@sdxHyperlinkEditorLinkTo);
  lbFileOrWebPage.Caption := cxGetResourceString(@sdxHyperlinkEditorFileOrWebPageLink);
  lbPlaceInDocument.Caption := cxGetResourceString(@sdxHyperlinkEditorPlaceInThisDocumentLink);
  lbEMail.Caption := cxGetResourceString(@sdxHyperlinkEditorEmailAddressLink);

  lcEditTextToDisplay.CaptionOptions.Text := cxGetResourceString(@sdxHyperlinkEditorTextToDisplay);
  lcEditScreenTip.CaptionOptions.Text := cxGetResourceString(@sdxHyperlinkEditorScreenTip);
  lcEditAddress.CaptionOptions.Text := cxGetResourceString(@sdxHyperlinkEditorAddress);
  lcEditEMailAddress.CaptionOptions.Text := cxGetResourceString(@sdxHyperlinkEditorEmailAddress);
  lcEditSubject.CaptionOptions.Text := cxGetResourceString(@sdxHyperlinkEditorSubject);
  lcEditCellRef.CaptionOptions.Text := cxGetResourceString(@sdxHyperlinkEditorCellReference);
  lcEditPlaceInDocument.CaptionOptions.Text := cxGetResourceString(@sdxHyperlinkEditorSelectPlace);
end;

function TdxSpreadSheetEditHyperlinkDialogForm.CreateEditingHyperlink: TdxSpreadSheetHyperlink;
begin
  Result := TdxSpreadSheetEditingHyperlink.Create(Self);
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.EditValueChanged(Sender: TObject);
begin
  if IsUpdating then
    Exit;
  SynchronizeDisplayText;
  InitializeHyperlinkValue;
  btnOK.Enabled := TdxSpreadSheetEditingHyperlink(EditingHyperlink).IsHyperlinkValid;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.edtAddressPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if ChooseFile.Execute then
    TcxTextEdit(Sender).Text := ChooseFile.FileName;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.edtAddressPropertiesChange(Sender: TObject);
var
  APos: Integer;
begin
  if IsUpdating then
    Exit;
  if Pos('www.', Trim(TcxTextEdit(Sender).Text)) = 1 then
  begin
    APos := TcxTextEdit(Sender).SelStart;
    TcxTextEdit(Sender).Text := URLPrefix + TcxTextEdit(Sender).Text;
    TcxTextEdit(Sender).SelStart := Length(URLPrefix) + APos;
  end;
  EditValueChanged(Sender);
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.edtEmailExit(Sender: TObject);
begin
  edtEmailPropertiesChange(Sender);
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.edtEmailPropertiesChange(Sender: TObject);
var
  APos: Integer;
begin
  if IsUpdating then
    Exit;
  if (Pos(dxEMailHyperlinkMailtoPrefix, TcxTextEdit(Sender).Text) = 0) and
    (Length(FPrevText) <= Length(TcxTextEdit(Sender).Text)) then
  begin
    APos := TcxTextEdit(Sender).SelStart;
    TcxTextEdit(Sender).Text := dxEMailHyperlinkMailtoPrefix + TcxTextEdit(Sender).Text;
    TcxTextEdit(Sender).SelStart := Length(dxEMailHyperlinkMailtoPrefix) + APos;
  end;
  FPrevText := TcxTextEdit(Sender).Text;
  EditValueChanged(Sender);
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.edtScreenTipPropertiesChange(Sender: TObject);
begin
  if IsUpdating then
    Exit;
  EditingHyperlink.ScreenTip := TcxTextEdit(Sender).Text;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.EditDisplayTextChanged(Sender: TObject);
begin
  if IsUpdating then
    Exit;
  DisplayTextChanged := True;
  if TcxTextEdit(Sender).Enabled then
    EditingHyperlink.DisplayText := TcxTextEdit(Sender).Text;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.SetupControls;
begin
  if Hyperlink <> nil then
    ActivePage := Hyperlink.ValueType
  else
    ActivePage := hvtFileOrWebPage;

  PopulateNames;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.SetEditFontStyle(AEdit: TcxCustomEdit; AStyles: TFontStyles);
begin
  if AEdit = nil then
    Exit;
  AEdit.Style.Font.Style := AStyles;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.SynchronizeDisplayText;
begin
  if IsUpdating or DisplayTextChanged or not edtTextToDisplay.Enabled then
    Exit;
  edtTextToDisplay.Text := GetDefaultDisplayText;
  DisplayTextChanged := False;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.tvDocumentPlaceChange(Sender: TObject; Node: TTreeNode);
begin
  if IsUpdating then
    Exit;
  btnOK.Enabled := Node.Level = 1;
  if btnOK.Enabled then
  begin
    SynchronizeDisplayText;
    EditValueChanged(nil);
  end;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.Initialize(
  ASheet: TdxSpreadSheetTableView; AHyperlink: TdxSpreadSheetHyperlink);
begin
  FSheet := ASheet;
  FHyperlink := AHyperlink;
  FEditingHyperlink := CreateEditingHyperlink;
  if Hyperlink <> nil then
    FEditingHyperlink.Assign(FHyperlink)
  else
    if ActiveContainer <> nil then
      TdxSpreadSheetEditingHyperlink(FEditingHyperlink).SetArea(cxInvalidRect)
    else
      TdxSpreadSheetEditingHyperlink(FEditingHyperlink).SetArea(Sheet.Selection.Area);

  cxImage1.Style.BorderStyle := ebsNone;
  cxImage2.Style.BorderStyle := ebsNone;
  cxImage3.Style.BorderStyle := ebsNone;
  SetupControls;
  if TdxSpreadSheetEditingHyperlink(EditingHyperlink).ValueType = hvtReference then
    edtCellRef.Text := GetReferenceText
  else
    edtCellRef.Text := dxReferenceToString(cxNullRect, SpreadSheet.OptionsView.R1C1Reference);
  Load;
  Modified := False;
  btnOK.Enabled := TdxSpreadSheetEditingHyperlink(EditingHyperlink).IsHyperlinkValid;
  ApplyLocalization;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.InitializeHyperlinkValue;
begin
  if not IsUpdating then
    EditingHyperlink.Value := GetValue;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.PopulateNames;

  function CheckCaption(const AValue: string): string;
  begin
    Result := AValue;
    if Pos(' ', Result) > 0 then
      Result := '''' + Result + '''';
  end;

var
  AName: TdxSpreadSheetDefinedName;
  I: Integer;
begin
  tvDocumentPlace.Items.BeginUpdate;
  try
    tvDocumentPlace.Items.Clear;
    FSheets := tvDocumentPlace.Items.AddChild(nil, cxGetResourceString(@sdxHyperlinkEditorCellReferenceNode));
    FNames := tvDocumentPlace.Items.AddChild(nil, cxGetResourceString(@sdxHyperlinkEditorDefinedNamesNode));

    for I := 0 to SpreadSheet.SheetCount - 1 do
    begin
      if SpreadSheet.Sheets[I] is TdxSpreadSheetTableView then
        tvDocumentPlace.Items.AddChildObject(FSheets, CheckCaption(SpreadSheet.Sheets[I].Caption), SpreadSheet.Sheets[I]);
    end;

    for I := 0 to SpreadSheet.DefinedNames.Count - 1 do
    begin
      AName := SpreadSheet.DefinedNames[I];
      if TdxSpreadSheetDefinedNameAccess(AName).IsCellReference then
        tvDocumentPlace.Items.AddChildObject(FNames, AName.ToString, AName);
    end;
  finally
    tvDocumentPlace.Items.EndUpdate;
    tvDocumentPlace.FullExpand;
  end;
end;

function TdxSpreadSheetEditHyperlinkDialogForm.GetActiveCell: TdxSpreadSheetCell;
begin
  Result := Sheet.Cells[Area.Top, Area.Left];
end;

function TdxSpreadSheetEditHyperlinkDialogForm.GetActiveContainer: TdxSpreadSheetContainer;
begin
  Result := Sheet.Controller.FocusedContainer;
  if (Result <> nil) and (Hyperlink <> Result.Hyperlink) then
    Result := nil;
end;

function TdxSpreadSheetEditHyperlinkDialogForm.GetArea: TRect;
begin
  Result := EditingHyperlink.Area;
end;

function TdxSpreadSheetEditHyperlinkDialogForm.GetDefaultDisplayText: string;
begin
  Result := dxSpreadSheetFormulaExcludeEqualSymbol(GetValue);
  if Pos(URLPrefix, Result) = 1 then
    Delete(Result, 1, Length(URLPrefix))
  else
    if Pos(dxEMailHyperlinkMailtoPrefix, Result) = 1 then
      Delete(Result, 1, Length(dxEMailHyperlinkMailtoPrefix));
end;

function TdxSpreadSheetEditHyperlinkDialogForm.GetPageLink(APage: TdxSpreadSheetHyperlinkValueType): TcxCustomEdit;
begin
  case APage of
    hvtReference:
      Result := lbPlaceInDocument;
    hvtEMail:
      Result := lbEMail;
  else
    Result := lbFileOrWebPage;
  end;
end;

function TdxSpreadSheetEditHyperlinkDialogForm.GetReferenceText: string;
begin
  Result := dxReferenceToString(TdxSpreadSheetEditingHyperlink(EditingHyperlink).ReferenceArea, SpreadSheet.OptionsView.R1C1Reference);
end;

function TdxSpreadSheetEditHyperlinkDialogForm.GetSelectedCell: TdxSpreadSheetCell;
begin
  if Sheet.Selection.FocusedContainer <> nil then
    Result := nil
  else
    Result := Sheet.Selection.FocusedCell;
end;

function TdxSpreadSheetEditHyperlinkDialogForm.GetSelectedNode: TTreeNode;

  function TryGetChild(AParent: TTreeNode; AIndex: Integer): TTreeNode;
  begin
    if AIndex < AParent.Count then
      Result := AParent[AIndex]
    else
      Result := AParent;
  end;

begin
  Result := tvDocumentPlace.Selected;
  if FSheets <> FNames then
  begin
    if TdxSpreadSheetEditingHyperlink(EditingHyperlink).ReferenceName <> nil then
      Result := TryGetChild(FNames, TdxSpreadSheetDefinedName(TdxSpreadSheetEditingHyperlink(EditingHyperlink).ReferenceName).Index)
    else
      if TdxSpreadSheetEditingHyperlink(EditingHyperlink).ReferenceSheet <> nil then
        Result := TryGetChild(FSheets,
          TdxSpreadSheetTableView(TdxSpreadSheetEditingHyperlink(EditingHyperlink).ReferenceSheet).Index)
      else
        Result := FSheets[0];
  end;
end;

function TdxSpreadSheetEditHyperlinkDialogForm.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := Sheet.SpreadSheet;
end;

function TdxSpreadSheetEditHyperlinkDialogForm.GetValue: string;

  function CheckExtraChars(const AText: string): string;
  begin
    if Pos(' ', AText) > 0 then
      Result := '''' + AText + ''''
    else
      Result := AText;
  end;

begin
  case ActivePage of
    hvtReference:
      begin
        Result := edtCellRef.Text;
        if tvDocumentPlace.Selected <> nil then
        begin
          if TObject(tvDocumentPlace.Selected.Data) is TdxSpreadSheetDefinedName then
            Result := CheckExtraChars(TdxSpreadSheetDefinedName(tvDocumentPlace.Selected.Data).ToString)
          else
            if TObject(tvDocumentPlace.Selected.Data) is TdxSpreadSheetCustomView then
              Result := CheckExtraChars(TdxSpreadSheetTableViewAccess(tvDocumentPlace.Selected.Data).Caption) + '!' + Result;
        end;
        Result := dxSpreadSheetFormulaIncludeEqualSymbol(Result);
      end;

    hvtEMail:
      begin
        Result := edtEmail.Text;
        if Trim(edtSubject.Text) <> '' then
          Result := Result + dxEMailHyperlinkSubjectPrefix + Trim(edtSubject.Text);
      end;
  else
    Result := edtAddress.Text;
  end;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.SetActivePage(const AValue: TdxSpreadSheetHyperlinkValueType);
begin
  lcMain.BeginUpdate;
  try
    if FActivePage <> AValue then
      SetEditFontStyle(GetPageLink(FActivePage), []);
    FActivePage := AValue;
    lcEditAddress.Visible := ActivePage = hvtFileOrWebPage;
    lcEditCellRef.Visible := ActivePage = hvtReference;
    lcEditPlaceInDocument.Visible := ActivePage = hvtReference;
    lcEditSubject.Visible := ActivePage = hvtEMail;
    lcEditEMailAddress.Visible := ActivePage = hvtEMail;
    SetEditFontStyle(GetPageLink(FActivePage), [fsUnderline]);
    if HandleAllocated then
      case ActivePage of
        hvtEMail:
          PostMessage(Handle, WM_CHANGEFOCUS, TdxNativeInt(edtEmail), 0);
        hvtReference:
          begin
            PostMessage(Handle, WM_CHANGEFOCUS, TdxNativeInt(edtCellRef), 0);
            btnOK.Enabled := (tvDocumentPlace.Selected <> nil) and (tvDocumentPlace.Selected.Level = 1);
          end;
      else
        PostMessage(Handle, WM_CHANGEFOCUS, TdxNativeInt(edtAddress), 0);
      end;
  finally
    lcMain.EndUpdate;
  end;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.SetHotTrackPage(AValue: Integer);
begin
  if AValue = HottrackPage then
    Exit;
  if (FHottrackPage >= 0) and (FHottrackPage <> Byte(FActivePage)) then
    SetEditFontStyle(GetPageLink(TdxSpreadSheetHyperlinkValueType(HottrackPage)), []);
  FHottrackPage := AValue;
  if FHottrackPage >= 0 then
    SetEditFontStyle(GetPageLink(TdxSpreadSheetHyperlinkValueType(HottrackPage)), [fsUnderline]);
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.ChangeFocus(var Message: TMessage);
begin
  if TWinControl(Message.WParam).CanFocus then
    TWinControl(Message.WParam).SetFocus;
  SynchronizeDisplayText;
  InitializeHyperlinkValue;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.NavbarItemClick(Sender: TObject);
begin
  if ActivePage <> TdxSpreadSheetHyperlinkValueType(TComponent(Sender).Tag) then
    ActivePage := TdxSpreadSheetHyperlinkValueType(TComponent(Sender).Tag);
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.NavbarItemMouseEnter(Sender: TObject);
begin
  HottrackPage := TComponent(Sender).Tag;
end;

procedure TdxSpreadSheetEditHyperlinkDialogForm.NavbarItemMouseLeave(Sender: TObject);
var
  ASite, AWindow: TWinControl;
begin
  ASite := pnFileOrWebPage;
  case TControl(Sender).Tag of
    1:
      ASite := pnPlaceInDocument;
    2:
      ASite := pnEMail;
  end;
  AWindow := FindVCLWindow(GetMouseCursorPos);
  if (AWindow <> nil) and ((AWindow = ASite) or (AWindow.Parent = ASite)) then
  begin
    HottrackPage := ASite.Tag;
    Exit;
  end;
  HottrackPage := -1;
end;

end.
