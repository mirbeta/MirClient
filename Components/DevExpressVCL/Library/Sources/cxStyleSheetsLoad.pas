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

unit cxStyleSheetsLoad;

{$I cxVer.inc}

interface

uses
  Types, Variants, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, dxCore, cxClasses, cxStyles, ExtCtrls, cxStyleSheetEditor;

type
  TcxPredefinedStyleSheetsClass = class of TcxPredefinedStyleSheets;

  TcxPredefinedStyleSheets = class
  private
    FList: TList;
  protected
    procedure AddStyleSheet(AStyleSheet: TcxCustomStyleSheet);
    procedure AddStyleSheets; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure GetStyleSheetsByClass(AStyleSheetClass: TcxCustomStyleSheetClass; const AList: TList);
  end;

  TcxPredefinedStyleSheetsList = class
  private
    FList: TList;
    FStyleSheetClassComboBox: TComboBox;
    FStyleSheetsListBox: TListBox;
    FLoadButton: TButton;
    FPreview: TcxStyleSheetEditorPreview;
    procedure StyleSheetClassComboBoxClick(Sender: TObject);
    procedure StyleSheetsListBoxClick(Sender: TObject);
  protected
    procedure FreeAndNilItems;
    procedure UpdateButton;
  public
    constructor Create(AStyleSheetClassComboBox: TComboBox; AStyleSheetsListBox: TListBox;
      ALoadButton: TButton);
    destructor Destroy; override;
    function CurrentStyleSheet: TcxCustomStyleSheet;
    function CurrentStyleSheetClass: TcxCustomStyleSheetClass;
    procedure FillListBox;
  end;

  TfrmcxStyleSheetsLoad = class(TForm)
    pnlBottom: TPanel;
    Bevel: TBevel;
    pnlStyles: TPanel;
    pnlStyleSheetClasses: TPanel;
    lbStyleSheetClass: TLabel;
    cbStyleSheetClasses: TComboBox;
    lbStyleSheets: TListBox;
    pnlPreview: TPanel;
    pnlClient: TPanel;
    btnLoad: TButton;
    btnClose: TButton;
    Panel2: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    lbPreview: TLabel;
    pnlPreviewClient: TPanel;
    procedure FormCreate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function CurrentStyleSheetClass: TcxCustomStyleSheetClass;
    procedure SetShowPreview(Value: Boolean);
  end;

procedure RegisterPredefinedStyleSheets(APredefinedStyleSheetsClass: TcxPredefinedStyleSheetsClass);
procedure UnregisterPredefinedStyleSheets(APredefinedStyleSheetsClass: TcxPredefinedStyleSheetsClass);
procedure GetPredefinedStyleSheetClasses(AList: TList);

procedure ShowLoadStyleSheetsFromIniFile(const AIniFileName: string;
  AStyleRepository: TcxStyleRepository; AOwner: TComponent; const AStyleSheetList: TList;
  AStyleGetName: TcxStyleGetName);

procedure ShowLoadStyleSheetsFromPreDefineStyles(AStyleRepository: TcxStyleRepository;
  AOwner: TComponent; const AStyleSheetList: TList; AStyleGetName: TcxStyleGetName);

implementation

{$R *.dfm}

uses
  IniFiles;

var
  FPredefinedStyleSheetsList: TList = nil;

procedure RegisterPredefinedStyleSheets(APredefinedStyleSheetsClass: TcxPredefinedStyleSheetsClass);
begin
  if FPredefinedStyleSheetsList = nil then
    FPredefinedStyleSheetsList := TList.Create;
  if FPredefinedStyleSheetsList.IndexOf(TObject(APredefinedStyleSheetsClass)) = -1 then
    FPredefinedStyleSheetsList.Add(TObject(APredefinedStyleSheetsClass));
end;

procedure UnregisterPredefinedStyleSheets(APredefinedStyleSheetsClass: TcxPredefinedStyleSheetsClass);
begin
  if FPredefinedStyleSheetsList <> nil then
  begin
    FPredefinedStyleSheetsList.Remove(TObject(APredefinedStyleSheetsClass));
    if FPredefinedStyleSheetsList.Count = 0 then
      FreeAndNil(FPredefinedStyleSheetsList);
  end;
end;

procedure GetPredefinedStyleSheetClasses(AList: TList);
begin
  dxCopyList(FPredefinedStyleSheetsList, AList);
end;

{ TcxPredefinedStyleSheets }

constructor TcxPredefinedStyleSheets.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TcxPredefinedStyleSheets.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TcxPredefinedStyleSheets.GetStyleSheetsByClass(AStyleSheetClass: TcxCustomStyleSheetClass;
    const AList: TList);
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    if TcxCustomStyleSheetClass(TcxCustomStyleSheet(FList[I]).ClassType) = AStyleSheetClass then
      AList.Add(FList[I]);
end;

procedure TcxPredefinedStyleSheets.AddStyleSheet(AStyleSheet: TcxCustomStyleSheet);
begin
  if FList.IndexOf(AStyleSheet) = -1 then
    FList.Add(AStyleSheet);
end;

{ TcxPredefinedStyleSheetsList }

constructor TcxPredefinedStyleSheetsList.Create(AStyleSheetClassComboBox: TComboBox;
            AStyleSheetsListBox: TListBox; ALoadButton: TButton);
var
  I: Integer;
begin
  inherited Create;
  FList := TList.Create;
  FStyleSheetClassComboBox := AStyleSheetClassComboBox;
  FStyleSheetsListBox := AStyleSheetsListBox;
  FLoadButton := ALoadButton;
  FStyleSheetClassComboBox.OnClick := StyleSheetClassComboBoxClick;
  FStyleSheetsListBox.OnClick := StyleSheetsListBoxClick;
  if FPredefinedStyleSheetsList <> nil then
    for I := 0 to FPredefinedStyleSheetsList.Count - 1 do
      FList.Add(TcxPredefinedStyleSheetsClass(FPredefinedStyleSheetsList[I]).Create);
end;

destructor TcxPredefinedStyleSheetsList.Destroy;
begin
  FreeAndNil(FPreview);
  FreeAndNilItems;
  inherited;
end;

function TcxPredefinedStyleSheetsList.CurrentStyleSheet: TcxCustomStyleSheet;
begin
  with FStyleSheetsListBox do
    if ItemIndex > -1 then
      Result :=  TcxCustomStyleSheet(Items.Objects[ItemIndex])
    else
      Result := nil;
end;

function TcxPredefinedStyleSheetsList.CurrentStyleSheetClass: TcxCustomStyleSheetClass;
begin
  with FStyleSheetClassComboBox do
    if ItemIndex > - 1 then
      Result := TcxCustomStyleSheetClass(Items.Objects[ItemIndex])
    else
      Result := nil;
end;

procedure TcxPredefinedStyleSheetsList.FillListBox;
var
  AForm: TfrmcxStyleSheetsLoad;
  AList: TList;
  I: Integer;
  StyleSheet: TcxCustomStyleSheet;
  PreviewClass: TcxStyleSheetEditorPreviewClass;
begin
  AForm := TfrmcxStyleSheetsLoad(GetParentForm(FStyleSheetsListBox));
  if CurrentStyleSheetClass = nil then
  begin
    FStyleSheetsListBox.Items.Clear;
    //AForm.Width := AForm.pnlStyles.Width;
  end
  else
  begin
    AList := TList.Create;
    try
      for I := 0 to FList.Count - 1 do
        TcxPredefinedStyleSheets(FList[I]).GetStyleSheetsByClass(CurrentStyleSheetClass, AList);

      with FStyleSheetsListBox.Items do
      begin
        BeginUpdate;
        try
          Clear;
          for I := 0 to AList.Count - 1 do
          begin
            StyleSheet := TcxCustomStyleSheet(AList[I]);
            AddObject(StyleSheet.Caption, StyleSheet);
          end;
        finally
          EndUpdate;
        end;
      end;
    finally
      AList.Free;
    end;
  end;

  FreeAndNil(FPreview);

  PreviewClass := nil;
  if CurrentStyleSheetClass <> nil then
    PreviewClass := GetPreviewByStyleSheetClass(CurrentStyleSheetClass);
  if PreviewClass <> nil then
  begin
    FPreview := PreviewClass.Create(AForm);
    AForm.SetShowPreview(True);
    FPreview.Control.Parent := AForm.pnlPreviewClient;
    FPreview.Control.Align := alClient;
    FPreview.SetStyleSheet(nil);

    if AForm.pnlPreviewClient.Width < FPreview.GetSize.X then
      AForm.Width := AForm.Width + FPreview.GetSize.X - AForm.pnlPreviewClient.Width;
    if AForm.pnlPreviewClient.Height < FPreview.GetSize.Y then
      AForm.Height := AForm.Height + FPreview.GetSize.Y - AForm.pnlPreviewClient.Height;
  end
  else
    AForm.SetShowPreview(False);

  UpdateButton;
end;

procedure TcxPredefinedStyleSheetsList.FreeAndNilItems;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TObject(FList[I]).Free;
  FList.Free;
end;

procedure TcxPredefinedStyleSheetsList.UpdateButton;
begin
  FLoadButton.Enabled := FStyleSheetsListBox.SelCount > 0;
end;

procedure TcxPredefinedStyleSheetsList.StyleSheetClassComboBoxClick(Sender: TObject);
begin
  FillListBox;
end;

procedure TcxPredefinedStyleSheetsList.StyleSheetsListBoxClick(Sender: TObject);
begin
  UpdateButton;
  if FPreview <> nil then
    FPreview.SetStyleSheet(CurrentStyleSheet);
end;

procedure ShowLoadStyleSheetsFromIniFile(const AIniFileName: string;
                AStyleRepository: TcxStyleRepository;
                AOwner: TComponent; const AStyleSheetList: TList;
                AStyleGetName: TcxStyleGetName);

  procedure SelectAllItems(AForm: TfrmcxStyleSheetsLoad);
  var
    I: Integer;
  begin
    for I := 0 to AForm.lbStyleSheets.Items.Count - 1 do
      AForm.lbStyleSheets.Selected[I] := True;
  end;

var
  AForm: TfrmcxStyleSheetsLoad;
  I: Integer;
  AIniFile: TMemIniFile;
  AStrings: TStringList;
begin
  AIniFile := TMemIniFile.Create(AIniFileName);
  AForm := TfrmcxStyleSheetsLoad.Create(nil);
  AForm.SetShowPreview(False);
  AStrings := TStringList.Create;
  try
    AIniFile.ReadSections(AForm.lbStyleSheets.Items);
    SelectAllItems(AForm);
    AForm.btnLoad.Enabled := (AForm.CurrentStyleSheetClass <> nil) and
        (AForm.lbStyleSheets.Items.Count > 0);
    AForm.ShowModal;
    if AForm.ModalResult = mrOk then
    begin
      for I := 0 to AForm.lbStyleSheets.Items.Count - 1 do
         if AForm.lbStyleSheets.Selected[I] then
           AStrings.Add(AForm.lbStyleSheets.Items[I]);
      LoadStyleSheetsFromIniFile(AIniFileName, AStyleRepository,
       AForm.CurrentStyleSheetClass,
       AStrings, AOwner, AStyleSheetList, AStyleGetName);
    end;
  finally
    AStrings.Free;
    AForm.Free;
    AIniFile.Free;
  end;
end;

procedure CreateStyleSheetByPredefine(AStyleRepository: TcxStyleRepository;
                AOwner: TComponent; const AStyleSheetList: TList;
                AStyleGetName: TcxStyleGetName; ASource: TcxCustomStyleSheet);
var
  AStyleSheet: TcxCustomStyleSheet;
begin
  AStyleSheet := AStyleRepository.CreateStyleSheetEx(TcxCustomStyleSheetClass(ASource.ClassType),
        AOwner);
  if AStyleSheetList <> nil then
    AStyleSheetList.Add(AStyleSheet);
  if AOwner.FindComponent(ASource.Name) = nil then
    AStyleSheet.Name := ASource.Name;
  AStyleSheet.Caption := ASource.Caption;
  CreateStyleSheetStyles(AStyleSheet, ASource, AStyleGetName);
end;

procedure ShowLoadStyleSheetsFromPreDefineStyles(AStyleRepository: TcxStyleRepository;
                AOwner: TComponent; const AStyleSheetList: TList;
                AStyleGetName: TcxStyleGetName);
var
  AForm: TfrmcxStyleSheetsLoad;
  APredefinedList: TcxPredefinedStyleSheetsList;
  I: Integer;
begin
  AForm := TfrmcxStyleSheetsLoad.Create(nil);
  APredefinedList := TcxPredefinedStyleSheetsList.Create(
      AForm.cbStyleSheetClasses, AForm.lbStyleSheets, AForm.btnLoad);
  try
    APredefinedList.FillListBox;
    AForm.ShowModal;
    if AForm.ModalResult = mrOk then
    begin
      for I := 0 to AForm.lbStyleSheets.Items.Count - 1 do
         if AForm.lbStyleSheets.Selected[I] then
           CreateStyleSheetByPredefine(AStyleRepository,
             AOwner, AStyleSheetList, AStyleGetName,
             TcxCustomStyleSheet(AForm.lbStyleSheets.Items.Objects[I]));
    end;
  finally
    APredefinedList.Free;
    AForm.Free;
  end;
end;

{ TfrmcxStyleSheetsLoad }

constructor TfrmcxStyleSheetsLoad.Create(AOwner: TComponent);
var
  I: Integer;
  AList: TList;
  StyleSheetClass: TcxCustomStyleSheetClass;
begin
  inherited Create(AOwner);
  AList := TList.Create;
  try
    GetRegisteredStyleSheetClasses(AList);
    for I := 0 to AList.Count - 1 do
    begin
      StyleSheetClass := TcxCustomStyleSheetClass(AList[I]);
      cbStyleSheetClasses.Items.AddObject(StyleSheetClass.ClassName, TObject(StyleSheetClass));
    end;

    with cbStyleSheetClasses do
      if Items.Count > 0 then ItemIndex := 0;
  finally
    AList.Free;
  end;
end;

function TfrmcxStyleSheetsLoad.CurrentStyleSheetClass: TcxCustomStyleSheetClass;
begin
  with cbStyleSheetClasses do
    if ItemIndex > - 1 then
      Result := TcxCustomStyleSheetClass(Items.Objects[ItemIndex])
    else
      Result := nil;
end;

procedure TfrmcxStyleSheetsLoad.SetShowPreview(Value: Boolean);
begin
  if pnlPreview.Visible <> Value then
    if Value then
    begin
      Constraints.MaxWidth := 0;
      ClientWidth := ClientWidth + pnlPreview.Width;
      pnlPreview.Visible := Value;
    end
    else
    begin
      pnlPreview.Visible := Value;
      ClientWidth := ClientWidth - pnlPreview.Width;
      Constraints.MaxWidth := Width;
    end;
end;

procedure TfrmcxStyleSheetsLoad.FormCreate(Sender: TObject);
begin
  lbStyleSheetClass.Height := cbStyleSheetClasses.Height;
  lbPreview.Height := cbStyleSheetClasses.Height;
end;

end.
