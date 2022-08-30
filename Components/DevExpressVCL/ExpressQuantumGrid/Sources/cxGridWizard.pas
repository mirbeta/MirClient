{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridWizard;


{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, DB, cxGraphics, cxControls, cxStyles, cxLookAndFeels,
  cxLookAndFeelPainters, dxCustomWizardControl, dxWizardControl, cxClasses, dxCore, cxGrid, cxGridLevel,
  cxGridCustomView, cxGridWizardStrs, cxGridWizardCustomPage, cxGridWizardCustomHelper, cxGridWizardTableViewHelper,
  cxGridWizardLayoutViewHelper, cxGridWizardBandedTableViewHelper, cxGridWizardChartViewHelper,
  cxGridWizardCardViewHelper, dxWizardControlForm, Dialogs, cxGridWizardServerModeViewHelper;

type
  { TcxGridWizardForm }

  TcxGridWizardForm = class(TdxWizardControlForm, IcxGridWizardFormActions)
    grPreviewGrid: TcxGrid;
    grPreviewGridLevel: TcxGridLevel;
    wcMain: TdxWizardControl;
    wcpFinish: TdxWizardControlPage;
    wcpStart: TdxWizardControlPage;
    wcpWelcome: TdxWizardControlPage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure wcMainButtonClick(Sender: TObject; AKind: TdxWizardControlButtonKind; var AHandled: Boolean);
    procedure wcMainPageChanged(Sender: TObject);
    procedure wcMainPageChanging(Sender: TObject; ANewPage: TdxWizardControlCustomPage; var AAllow: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FDeleteExistingStructure: Boolean;
    FGrid: TcxCustomGrid;
    FHelper: TcxGridWizardCustomHelper;
    FIsCustomFormSize: Boolean;
    FIsDetail: Boolean;
    FIsDetailViewCustomizing: Boolean;
    FIsMultiLevelStructure: Boolean;
    FIsNextLevelNeeded: Boolean;
    FMasterGridView: TcxCustomGridView;
    FMultiLevelStructureMasterViewName: string;
    FSelectedGridView: TcxCustomGridView;
    FWasDetail: Boolean;

    procedure AdjustFormConstraints;
    procedure ConfirmNextLevelCreation;
    procedure CreatePageContent(APage: TdxWizardControlCustomPage; AFrameClass: TcxGridWizardCustomPageFrameClass);
    procedure DestroyHelpersPages;
    procedure ExecuteGridWizardForNextLevelCreation;
    function GetPageFrame(APage: TdxWizardControlCustomPage): TcxGridWizardCustomPageFrame;
    procedure Initialize;
    procedure InitializeCreationMode;
    procedure InitializeEditingMode;
    procedure LocalizePage(APage: TdxWizardControlCustomPage);
    procedure SaveGridView;
    procedure UpdateCaption;

    // Messages
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure WMSizing(var AMessage: TMessage); message WM_SIZING;
  protected
    // IcxGridWizardFormActions
    procedure CreatePagesContent;
    procedure CreateHelperForGridView(AGridViewClass: TClass);
    function GetCustomizedGrid: TcxCustomGrid;
    function GetIsDetailViewCustomizing: Boolean;
    function GetIsMultiLevelStructure: Boolean;
    function GetMultiLevelStructureMasterViewName: string;
    function GetPreviewGrid: TcxGrid;
    function GetSelectedGridView: TcxCustomGridView;
    function GetHelper: TcxGridWizardCustomHelper;
    procedure JumpToNextPage;
    procedure RefreshPreviewGridContent;
    procedure SetDeleteExistingStructure(const AValue: Boolean);
    procedure SetIsDetail(const AValue: Boolean);
    procedure SetIsMultiLevelStructure(const AValue: Boolean);
    procedure SetMasterGridView(AValue: TcxCustomGridView);
    procedure UpdateButtonsState;
  end;

procedure ExecuteGridWizard(AGrid: TcxCustomGrid; ASelectedGridView: TcxCustomGridView = nil);

implementation

uses
  cxGridWizardCommonWizardModePage, cxGridWizardCommonSelectViewPage, cxGridWizardCommonFinishPage, cxDWMApi, cxGeometry, Math;

{$R *.dfm}

procedure ExecuteGridWizard(AGrid: TcxCustomGrid; ASelectedGridView: TcxCustomGridView = nil);
var
  AForm: TcxGridWizardForm;
begin
  AForm := TcxGridWizardForm.Create(nil);
  try
    AForm.FGrid := AGrid;
    AForm.FSelectedGridView := ASelectedGridView;
    AForm.Initialize;
    if AForm.ShowModal = mrOk then
      SetDesignerModified(AGrid);
  finally
    AForm.Free;
  end;
end;

{ TcxGridWizardForm }

procedure TcxGridWizardForm.AdjustFormConstraints;
begin
end;

procedure TcxGridWizardForm.Initialize;
begin
  if FSelectedGridView = nil then
    InitializeCreationMode
  else
    InitializeEditingMode;
end;

procedure TcxGridWizardForm.InitializeCreationMode;
begin
  CreatePageContent(wcpWelcome, TcxGridWizardCommonWizardModePageFrame);
  CreatePageContent(wcpStart, TcxGridWizardCommonSelectViewPageFrame);
  CreatePageContent(wcpFinish, TcxGridWizardCommonFinishPageFrame);
  GetPageFrame(wcpWelcome).LoadSettings;
  FWasDetail := False;
  FIsDetailViewCustomizing := False;
end;

procedure TcxGridWizardForm.InitializeEditingMode;
begin
  CreateHelperForGridView(FSelectedGridView.ClassType);
  FHelper.InitializeEditingMode(FSelectedGridView, grPreviewGrid.ActiveView);
  FHelper.SaveGridViewOptionsData;

  CreatePagesContent;
  CreatePageContent(wcpFinish, TcxGridWizardCommonFinishPageFrame);
  wcMain.DeletePage(wcpWelcome);
  wcMain.DeletePage(wcpStart);

  FWasDetail := FSelectedGridView.IsDetail;
end;

procedure TcxGridWizardForm.ConfirmNextLevelCreation;
begin
  FMultiLevelStructureMasterViewName := FGrid.ActiveLevel.GridView.Name;
  FIsNextLevelNeeded := FIsMultiLevelStructure;
  if FIsNextLevelNeeded then
    FIsNextLevelNeeded := MessageDlg(cxGetResourceString(@scxgwCommonCreateDetailLevelQuery), mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TcxGridWizardForm.CreatePageContent(
  APage: TdxWizardControlCustomPage; AFrameClass: TcxGridWizardCustomPageFrameClass);
var
  AFrame: TcxGridWizardCustomPageFrame;
begin
  AFrame := AFrameClass.Create(Self);
  AFrame.Align := alClient;
  AFrame.Parent := APage;
  AFrame.lcMain.HandleNeeded;

  APage.Header.DescriptionVisibility := wcevAlwaysVisible;
  APage.Tag := TdxNativeInt(AFrame);

  LocalizePage(APage);
end;

procedure TcxGridWizardForm.ExecuteGridWizardForNextLevelCreation;
var
  AForm: TcxGridWizardForm;
begin
  AForm := TcxGridWizardForm.Create(nil);
  try
    AForm.FGrid := FGrid;
    AForm.FSelectedGridView := nil;
    AForm.InitializeCreationMode;
    AForm.FIsMultiLevelStructure := True;
    AForm.FIsDetailViewCustomizing := True;
    AForm.FMultiLevelStructureMasterViewName := FMultiLevelStructureMasterViewName;
    AForm.wcMain.DeletePage(AForm.wcpWelcome);
    AForm.ShowModal;
  finally
    AForm.Free;
  end;
end;

function TcxGridWizardForm.GetPageFrame(APage: TdxWizardControlCustomPage): TcxGridWizardCustomPageFrame;
begin
  if APage <> nil then
    Result := TcxGridWizardCustomPageFrame(APage.Tag)
  else
    Result := nil;
end;

procedure TcxGridWizardForm.LocalizePage(APage: TdxWizardControlCustomPage);
var
  AFrame: TcxGridWizardCustomPageFrame;
begin
  AFrame := GetPageFrame(APage);
  APage.Header.Title := AFrame.PageTitle;
  APage.Header.Description := AFrame.PageDescription;
  AFrame.ApplyLocalization;
end;

procedure TcxGridWizardForm.SaveGridView;
var
  I: Integer;
begin
  if (FSelectedGridView = nil) and FDeleteExistingStructure then
  begin
    for I := FGrid.Levels.Count - 1 downto 0 do
      FGrid.Levels.Items[I].Free;
  end;
  if FIsDetail then
    FHelper.SaveGridViewAsDetail(FSelectedGridView, FMasterGridView, FGrid)
  else
    if FWasDetail then
      FHelper.SaveGridViewAndDeleteDetailed(FSelectedGridView, FGrid)
    else
      FHelper.SaveGridView(FSelectedGridView, FGrid);
end;

procedure TcxGridWizardForm.CMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
  if (Message.Result = 0) and (Message.CharCode = VK_ESCAPE) then
  begin
    Close;
    Message.Result := 1;
  end;
end;

procedure TcxGridWizardForm.WMSizing(var AMessage: TMessage);
begin
  inherited;
  FIsCustomFormSize := True;
end;

{ IcxGridWizardFormActions }

procedure TcxGridWizardForm.CreatePagesContent;
var
  I: Integer;
begin
  for I := 0 to FHelper.PageClassCount - 1 do
    CreatePageContent(wcMain.AddPage, TcxGridWizardCustomPageFrameClass(FHelper.PageClasses[I]));
  wcpFinish.PageIndex := wcMain.PageCount - 1;
end;

procedure TcxGridWizardForm.CreateHelperForGridView(AGridViewClass: TClass);
begin
  if FHelper <> nil then
  try
    DestroyHelpersPages;
  finally
    FHelper.Free;
  end;
  FHelper := cxGridWizardHelperInfoList.GetHelperClassBy(AGridViewClass).Create(FGrid);
  grPreviewGrid.ActiveLevel.GridView := grPreviewGrid.CreateView(FHelper.GetGridViewClass);
  UpdateCaption;
end;

procedure TcxGridWizardForm.DestroyHelpersPages;
var
  AList: TList;
  APage: TdxWizardControlCustomPage;
  I: Integer;
begin
  AList := TList.Create;
  try
    grPreviewGrid.Parent := wcpFinish;
    for I := 0 to FHelper.PageClassCount - 1 do
      AList.Add(FHelper.PageClasses[I]);

    for I := wcMain.PageCount - 1 downto 0 do
    begin
      APage := wcMain.Pages[I];
      if AList.IndexOf(GetPageFrame(APage).ClassType) >= 0 then
      begin
        GetPageFrame(APage).Free;
        wcMain.DeletePage(APage);
      end;
    end;
  finally
    AList.Free;
  end;
end;

function TcxGridWizardForm.GetCustomizedGrid: TcxCustomGrid;
begin
  Result := FGrid;
end;

function TcxGridWizardForm.GetIsDetailViewCustomizing: Boolean;
begin
  Result := FIsDetailViewCustomizing;
end;

function TcxGridWizardForm.GetIsMultiLevelStructure: Boolean;
begin
  Result := FIsMultiLevelStructure;
end;

function TcxGridWizardForm.GetMultiLevelStructureMasterViewName: string;
begin
  Result := FMultiLevelStructureMasterViewName;
end;

function TcxGridWizardForm.GetPreviewGrid: TcxGrid;
begin
  Result := grPreviewGrid;
end;

function TcxGridWizardForm.GetSelectedGridView: TcxCustomGridView;
begin
  Result := FSelectedGridView;
end;

function TcxGridWizardForm.GetHelper: TcxGridWizardCustomHelper;
begin
  Result := FHelper;
end;

procedure TcxGridWizardForm.JumpToNextPage;
var
  AHandled: Boolean;
begin
  wcMainButtonClick(wcMain, wcbkNext, AHandled);
  wcMain.GoToNextPage;
end;

procedure TcxGridWizardForm.RefreshPreviewGridContent;
var
  AActiveFrame: TcxGridWizardCustomPageFrame;
begin
  AActiveFrame := GetPageFrame(wcMain.ActivePage);
  if AActiveFrame <> nil then
  begin
    AActiveFrame.ApplySettings;
    FHelper.PreparePreview(grPreviewGrid.ActiveView);
  end;
end;

procedure TcxGridWizardForm.SetIsDetail(const AValue: Boolean);
begin
  FIsDetail := AValue;
end;

procedure TcxGridWizardForm.SetIsMultiLevelStructure(const AValue: Boolean);
begin
  FIsMultiLevelStructure := AValue;
end;

procedure TcxGridWizardForm.SetDeleteExistingStructure(const AValue: Boolean);
begin
  FDeleteExistingStructure := AValue;
end;

procedure TcxGridWizardForm.SetMasterGridView(AValue: TcxCustomGridView);
begin
  FMasterGridView := AValue;
end;

procedure TcxGridWizardForm.UpdateCaption;
begin
  if FSelectedGridView <> nil then
    Caption := Format(cxGetResourceString(@scxgwCommonWizardCaptionEditing), [FSelectedGridView.Name])
  else
    Caption := cxGetResourceString(@scxgwCommonWizardCaption);
end;

procedure TcxGridWizardForm.UpdateButtonsState;
var
  AActiveFrame: TcxGridWizardCustomPageFrame;
begin
  AActiveFrame := GetPageFrame(wcMain.ActivePage);
  if AActiveFrame <> nil then
  begin
    wcMain.Buttons.Back.Enabled := AActiveFrame.CanJumpToPrevPage;
    wcMain.Buttons.Next.Enabled := AActiveFrame.CanJumpToNextPage;
    wcMain.Buttons.Finish.Enabled := AActiveFrame.CanFinishWizard;
  end;
end;

{ TcxGridWizardForm events}

procedure TcxGridWizardForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrCancel then
    CanClose := MessageDlg(cxGetResourceString(@scxgwCommonCloseQuery), mtWarning, [mbYes, mbNo], 0) = mrYes;
end;

procedure TcxGridWizardForm.FormCreate(Sender: TObject);
begin
//  if cxIsVCLThemesEnabled and IsCompositionEnabled then
//    wcMain.ViewStyle := wcvsAero;
end;

procedure TcxGridWizardForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHelper);
  if FIsNextLevelNeeded then
    ExecuteGridWizardForNextLevelCreation;
end;

procedure TcxGridWizardForm.FormShow(Sender: TObject);
begin
  AdjustFormConstraints;
end;

procedure TcxGridWizardForm.wcMainButtonClick(Sender: TObject; AKind: TdxWizardControlButtonKind; var AHandled: Boolean);
var
  I: Integer;
begin
  case AKind of
    wcbkCancel:
      Close;

    wcbkBack, wcbkNext:
      begin
        GetPageFrame(wcMain.ActivePage).ApplySettings;
        for I := 0 to wcMain.PageCount - 1 do
          GetPageFrame(wcMain.Pages[I]).GridViewChanged;
        for I := 0 to wcMain.PageCount - 1 do
          wcMain.Pages[I].PageVisible := GetPageFrame(wcMain.Pages[I]).Visible;
      end;

    wcbkFinish:
      begin
        if wcMain.ActivePage = wcpFinish then
          FHelper.Assign(grPreviewGrid.ActiveView)
        else
        begin
          GetPageFrame(wcMain.ActivePage).ApplySettings;
          FHelper.RestoreGridViewOptionsData;
        end;
        SaveGridView;
        ConfirmNextLevelCreation;
        ModalResult := mrOk;
      end;
  end;
end;

procedure TcxGridWizardForm.wcMainPageChanged(Sender: TObject);
begin
  AdjustFormConstraints;
  if GetPageFrame(wcMain.ActivePage) <> nil then
  begin
    GetPageFrame(wcMain.ActivePage).PageActivating;
    UpdateButtonsState;
  end;
end;

procedure TcxGridWizardForm.wcMainPageChanging(Sender: TObject; ANewPage: TdxWizardControlCustomPage; var AAllow: Boolean);
begin
  if GetPageFrame(wcMain.ActivePage) <> nil then
    GetPageFrame(wcMain.ActivePage).PageDeactivating;
  if GetPageFrame(ANewPage) <> nil then
    GetPageFrame(ANewPage).LoadSettings;
end;

end.
