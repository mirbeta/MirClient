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

unit cxGridWizardCustomPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, DB, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, ComCtrls, StdCtrls, cxGrid, dxCustomWizardControl, dxWizardControl,
  cxGridWizardCustomHelper, cxGridCustomView, dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels, cxClasses,
  cxCheckComboBox, cxCheckBox;

type
  { IcxGridWizardFormActions }

  IcxGridWizardFormActions = interface
  ['{7DBBCA0B-43CD-438A-BF32-16BD078F063C}']
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

  { TcxGridWizardCustomPageFrame }

  TcxGridWizardCustomPageFrame = class(TFrame)
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
  private
    FFormActions: IcxGridWizardFormActions;

    function GetCustomizedGrid: TcxCustomGrid;
    function GetHelperInfoList: TcxGridWizardHelperInfoList;
    function GetIsDetailViewCustomizing: Boolean;
    function GetIsMultiLevelStructure: Boolean;
    function GetMultiLevelStructureMasterViewName: string;
    function GetPreviewGrid: TcxGrid;
    function GetSelectedGridView: TcxCustomGridView;
    function GetHelper: TcxGridWizardCustomHelper;
    procedure SetIsMultiLevelStructure(const AValue: Boolean);
  protected
    // Messages
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

    procedure CreatePagesContent;
    procedure CreateHelperForGridView(AGridViewClass: TClass);
    function GetCanFinishWizard: Boolean; virtual;
    function GetCanJumpToNextPage: Boolean; virtual;
    function GetCanJumpToPrevPage: Boolean; virtual;
    function GetPageDescription: string; virtual;
    function GetPageTitle: string; virtual;
    procedure JumpToNextPage;
    procedure RefreshPreviewGridContent;
    procedure SetDeleteExistingStructure(const AValue: Boolean);
    procedure SetIsDetail(const AValue: Boolean);
    procedure SetMasterGridView(AValue: TcxCustomGridView);
    procedure UpdateOwnerButtonsState;

    function GetCheckComboBoxValue(ACombo: TcxCheckComboBox): string;
    procedure SetCheckComboBoxValue(ACombo: TcxCheckComboBox; const AText: string);

    property CustomizedGrid: TcxCustomGrid read GetCustomizedGrid;
    property HelperInfoList: TcxGridWizardHelperInfoList read GetHelperInfoList;
    property MultiLevelStructureMasterViewName: string read GetMultiLevelStructureMasterViewName;
    property IsDetailViewCustomizing: Boolean read GetIsDetailViewCustomizing;
    property IsMultiLevelStructure: Boolean read GetIsMultiLevelStructure write SetIsMultiLevelStructure;
    property PreviewGrid: TcxGrid read GetPreviewGrid;
    property SelectedGridView: TcxCustomGridView read GetSelectedGridView;
    property Helper: TcxGridWizardCustomHelper read GetHelper;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ApplyLocalization; virtual;
    procedure ApplySettings; virtual;
    procedure GridViewChanged; virtual;
    procedure LoadSettings; virtual;
    procedure PageActivating; virtual;
    procedure PageDeactivating; virtual;

    property CanFinishWizard: Boolean read GetCanFinishWizard;
    property CanJumpToNextPage: Boolean read GetCanJumpToNextPage;
    property CanJumpToPrevPage: Boolean read GetCanJumpToPrevPage;
    property PageDescription: string read GetPageDescription;
    property PageTitle: string read GetPageTitle;
  end;

  TcxGridWizardCustomPageFrameClass = class of TcxGridWizardCustomPageFrame;

implementation

{$R *.dfm}

{ TcxGridWizardCustomPageFrame }

constructor TcxGridWizardCustomPageFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFormActions := AOwner as IcxGridWizardFormActions;
end;

procedure TcxGridWizardCustomPageFrame.ApplyLocalization;
begin
  //do nothing
end;

procedure TcxGridWizardCustomPageFrame.ApplySettings;
begin
  //do nothing
end;

procedure TcxGridWizardCustomPageFrame.GridViewChanged;
begin
  //do nothing
end;

procedure TcxGridWizardCustomPageFrame.LoadSettings;
begin
  //do nothing
end;

procedure TcxGridWizardCustomPageFrame.PageActivating;
begin
  //do nothing
end;

procedure TcxGridWizardCustomPageFrame.PageDeactivating;
begin
  //do nothing
end;

procedure TcxGridWizardCustomPageFrame.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  cxPaintCanvas.BeginPaint(Message.DC);
  try
    cxDrawTransparentControlBackground(Self, cxPaintCanvas, ClientRect);
  finally
    cxPaintCanvas.EndPaint;
  end;
  Message.Result := 1;
end;

procedure TcxGridWizardCustomPageFrame.CreatePagesContent;
begin
  FFormActions.CreatePagesContent;
end;

procedure TcxGridWizardCustomPageFrame.CreateHelperForGridView(AGridViewClass: TClass);
begin
  FFormActions.CreateHelperForGridView(AGridViewClass);
end;

function TcxGridWizardCustomPageFrame.GetCanFinishWizard: Boolean;
begin
  Result := CanJumpToNextPage;
end;

function TcxGridWizardCustomPageFrame.GetCanJumpToNextPage: Boolean;
begin
  Result := True;
end;

function TcxGridWizardCustomPageFrame.GetCanJumpToPrevPage: Boolean;
begin
  Result := True;
end;

function TcxGridWizardCustomPageFrame.GetPageDescription: string;
begin
  Result := EmptyStr;
end;

function TcxGridWizardCustomPageFrame.GetPageTitle: string;
begin
  Result := EmptyStr;
end;

procedure TcxGridWizardCustomPageFrame.JumpToNextPage;
begin
  FFormActions.JumpToNextPage;
end;

procedure TcxGridWizardCustomPageFrame.RefreshPreviewGridContent;
begin
  FFormActions.RefreshPreviewGridContent;
end;

procedure TcxGridWizardCustomPageFrame.SetDeleteExistingStructure(const AValue: Boolean);
begin
  FFormActions.SetDeleteExistingStructure(AValue);
end;

procedure TcxGridWizardCustomPageFrame.SetIsDetail(const AValue: Boolean);
begin
  FFormActions.SetIsDetail(AValue);
end;

procedure TcxGridWizardCustomPageFrame.SetMasterGridView(AValue: TcxCustomGridView);
begin
  FFormActions.SetMasterGridView(AValue);
end;

procedure TcxGridWizardCustomPageFrame.UpdateOwnerButtonsState;
begin
  FFormActions.UpdateButtonsState;
end;

function TcxGridWizardCustomPageFrame.GetCheckComboBoxValue(ACombo: TcxCheckComboBox): string;
begin
  if (ACombo.Properties.EditValueFormat = cvfStatesString) or
    ((ACombo.Properties.EditValueFormat = cvfInteger) and (ACombo.EditValue <> 0)) then
    Result := ACombo.Text
  else
    Result := EmptyStr;
end;

procedure TcxGridWizardCustomPageFrame.SetCheckComboBoxValue(ACombo: TcxCheckComboBox; const AText: string);
var
  AList: TStringList;
  AValue: Integer;
  I: Integer;
begin
  AList := TStringList.Create;
  try
    AList.CaseSensitive := False;
    AList.Text := StringReplace(AText, ';', #13#10, [rfReplaceAll]);

    AValue := 0;
    for I := 0 to ACombo.Properties.Items.Count - 1 do
    begin
      if AList.IndexOf(ACombo.Properties.Items[I].Description) >= 0 then
        AValue := AValue or (1 shl I);
    end;
    ACombo.EditValue := AValue;
  finally
    AList.Free;
  end;
end;

function TcxGridWizardCustomPageFrame.GetCustomizedGrid: TcxCustomGrid;
begin
  Result := FFormActions.GetCustomizedGrid;
end;

function TcxGridWizardCustomPageFrame.GetHelperInfoList: TcxGridWizardHelperInfoList;
begin
  Result := cxGridWizardHelperInfoList;
end;

function TcxGridWizardCustomPageFrame.GetIsDetailViewCustomizing: Boolean;
begin
  Result := FFormActions.GetIsDetailViewCustomizing;
end;

function TcxGridWizardCustomPageFrame.GetIsMultiLevelStructure: Boolean;
begin
  Result := FFormActions.GetIsMultiLevelStructure;
end;

function TcxGridWizardCustomPageFrame.GetMultiLevelStructureMasterViewName: string;
begin
  Result := FFormActions.GetMultiLevelStructureMasterViewName;
end;

function TcxGridWizardCustomPageFrame.GetPreviewGrid: TcxGrid;
begin
  Result := FFormActions.GetPreviewGrid;
end;

function TcxGridWizardCustomPageFrame.GetSelectedGridView: TcxCustomGridView;
begin
  Result := FFormActions.GetSelectedGridView;
end;

function TcxGridWizardCustomPageFrame.GetHelper: TcxGridWizardCustomHelper;
begin
  Result := FFormActions.GetHelper;
end;

procedure TcxGridWizardCustomPageFrame.SetIsMultiLevelStructure(const AValue: Boolean);
begin
  FFormActions.SetIsMultiLevelStructure(AValue);
end;

end.
