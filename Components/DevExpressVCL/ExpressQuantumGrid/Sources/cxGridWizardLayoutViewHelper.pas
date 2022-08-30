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

unit cxGridWizardLayoutViewHelper;

{$I cxVer.inc}

interface

uses
  SysUtils, Forms, cxGraphics, cxDBData, cxEdit, dxLayoutContainer,
  cxGridWizardCustomHelper, cxGridCustomView, cxGridLayoutView, cxGridViewLayoutContainer, cxGridDBLayoutView, cxGridLevel,
  dxCustomWizardControl, dxWizardControl, cxGrid, cxGridDBDataDefinitions;

type

  { TcxGridWizardCustomLayoutViewHelper }

  TcxGridWizardCustomLayoutViewHelper = class(TcxGridWizardCustomTableLikeViewHelper)
  private
    function GetGridView: TcxGridLayoutView;
    function GetItem(Index: Integer): TcxGridLayoutViewItem;
  protected
    procedure CheckComponentNames(AView: TcxCustomGridView); override;
  public
    function GetDefaultItemCaption: string; override;
    class function GetGridViewClass: TcxCustomGridViewClass; override;
    procedure InitializeEditingMode(AEditingView, APreviewView: TcxCustomGridView); override;
    procedure RestoreGridViewOptionsData; override;
    procedure SaveGridViewOptionsData; override;

    property Items[Index: Integer]: TcxGridLayoutViewItem read GetItem;
    property GridView: TcxGridLayoutView read GetGridView;
  end;

  { TcxGridWizardLayoutViewHelper }

  TcxGridWizardLayoutViewHelper = class(TcxGridWizardCustomLayoutViewHelper)
  protected
    function CanSaveData: Boolean; override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
  public
    procedure CorrectCustomizationFormContent(ACustomizationForm: TForm); override;
  end;

  { TcxGridWizardDBLayoutViewHelper }

  TcxGridWizardDBLayoutViewHelper = class(TcxGridWizardCustomLayoutViewHelper, IcxGridWizardHelperDBDataControllerSupport)
  private
    function GetGridView: TcxGridDBLayoutView;
    function GetItem(Index: Integer): TcxGridDBLayoutViewItem;
  protected
    function GetItemCaption(Index: Integer): string; override;
    function GetItemFieldName(Index: Integer): string; override;
    function GetPageClassCount: Integer; override;
    function GetPageClasses(Index: Integer): TClass; override;
    procedure SetItemFieldName(Index: Integer; const AValue: string); override;
    // IcxGridWizardHelperDBDataControllerSupport
    function GetDataController: TcxDBDataController;
  public
    procedure DeleteItem(const AFieldName: string); override;
    class function GetGridViewClass: TcxCustomGridViewClass; override;
    function GetItemIndexByFieldName(const AFieldName: string): Integer; override;
    //
    property Items[Index: Integer]: TcxGridDBLayoutViewItem read GetItem;
    property GridView: TcxGridDBLayoutView read GetGridView;
  end;

implementation

uses
  Classes, cxClasses, cxGridWizardCustomPage, cxGridWizardDBViewsDataSourcePage,
  cxGridWizardDBViewsSelectItemsForDisplayPage, cxGridWizardCommonOptionsFilteringSortingPage,
  cxGridWizardLayoutViewOptionsCustomizeItemsPage, cxGridWizardDBViewsDataLoadingSettingsPage,
  cxGridWizardLayoutViewOptionsViewSettingsPage, cxGridWizardLayoutViewOptionsCarouselModePage,
  cxGridWizardLayoutViewOptionsInterfaceElementsPage, cxGridWizardLayoutViewOptionsBehaviorPage,
  cxGridWizardUnboundViewsSelectItemsForDisplayPage, cxGridLayoutViewCustomizationForm, cxGridCommon;

const
  cxGridWizardLayoutViewPages: array [0..6] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardUnboundViewsSelectItemsForDisplayPageFrame, TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame,
    TcxGridWizardLayoutViewOptionsViewSettingsPageFrame, TcxGridWizardLayoutViewOptionsCarouselModePageFrame,
    TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame, TcxGridWizardCommonOptionsFilteringSortingPageFrame,
    TcxGridWizardLayoutViewOptionsBehaviorPageFrame);

  cxGridWizardDBLayoutViewPages: array [0..8] of TcxGridWizardCustomPageFrameClass = (
    TcxGridWizardDBViewsDataSourcePageFrame, TcxGridWizardDBViewsSelectItemsForDisplayPageFrame,
    TcxGridWizardLayoutViewOptionsCustomizeItemsPageFrame, TcxGridWizardDBViewsDataLoadingSettingsPageFrame,
    TcxGridWizardLayoutViewOptionsViewSettingsPageFrame, TcxGridWizardLayoutViewOptionsCarouselModePageFrame,
    TcxGridWizardLayoutViewOptionsInterfaceElementsPageFrame, TcxGridWizardCommonOptionsFilteringSortingPageFrame,
    TcxGridWizardLayoutViewOptionsBehaviorPageFrame);

type
  TcxGridLayoutViewCustomizationFormAccess = class(TcxGridLayoutViewCustomizationForm);
  TcxGridCustomLayoutContainerAccess = class(TcxGridCustomLayoutContainer);

{$R cxGridWizardLayoutViewHelper.res}

{ TcxGridWizardCustomLayoutViewHelper }

function TcxGridWizardCustomLayoutViewHelper.GetDefaultItemCaption: string;
begin
  Result := 'Item';
end;

class function TcxGridWizardCustomLayoutViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridLayoutView;
end;

procedure TcxGridWizardCustomLayoutViewHelper.InitializeEditingMode(AEditingView, APreviewView: TcxCustomGridView);
begin
  inherited InitializeEditingMode(AEditingView, APreviewView);
  TcxGridCustomLayoutContainerAccess(GridView.Container).CustomizationHelper.ResetPattern;
end;

procedure TcxGridWizardCustomLayoutViewHelper.RestoreGridViewOptionsData;
begin
  GridView.OptionsData.Appending := FOriginalGridViewOptionsData.Appending;
  GridView.OptionsData.Deleting := FOriginalGridViewOptionsData.Deleting;
  GridView.OptionsData.Editing := FOriginalGridViewOptionsData.Editing;
  GridView.OptionsData.Inserting := FOriginalGridViewOptionsData.Inserting;
end;

procedure TcxGridWizardCustomLayoutViewHelper.SaveGridViewOptionsData;
begin
  FOriginalGridViewOptionsData.Appending := GridView.OptionsData.Appending;
  FOriginalGridViewOptionsData.Deleting := GridView.OptionsData.Deleting;
  FOriginalGridViewOptionsData.Editing := GridView.OptionsData.Editing;
  FOriginalGridViewOptionsData.Inserting := GridView.OptionsData.Inserting;
  GridView.OptionsData.Appending := False;
  GridView.OptionsData.Deleting := False;
  GridView.OptionsData.Editing := False;
  GridView.OptionsData.Inserting := False;
end;

procedure TcxGridWizardCustomLayoutViewHelper.CheckComponentNames(AView: TcxCustomGridView);
var
  ALayoutView: TcxGridLayoutView;
  I: Integer;
begin
  ALayoutView := AView as TcxGridLayoutView;
  for I := 0 to ALayoutView.ItemCount - 1 do
    ALayoutView.Items[I].Name := CreateUniqueName(GetParentForm(ALayoutView.Control),
      ALayoutView, ALayoutView.Items[I], ScxGridPrefixName, '');
end;

function TcxGridWizardCustomLayoutViewHelper.GetGridView: TcxGridLayoutView;
begin
  Result := TcxGridLayoutView(inherited GetGridView);
end;

function TcxGridWizardCustomLayoutViewHelper.GetItem(Index: Integer): TcxGridLayoutViewItem;
begin
  Result := GridView.Items[Index];
end;

{ TcxGridWizardLayoutViewHelper }

procedure TcxGridWizardLayoutViewHelper.CorrectCustomizationFormContent(ACustomizationForm: TForm);
var
  ALayoutCustomizationForm: TcxGridLayoutViewCustomizationFormAccess;
begin
  inherited CorrectCustomizationFormContent(ACustomizationForm);
  ALayoutCustomizationForm := TcxGridLayoutViewCustomizationFormAccess(ACustomizationForm as TcxGridLayoutViewCustomizationForm);
  ALayoutCustomizationForm.PreviewView.Navigator.Visible := True;
  ALayoutCustomizationForm.PreviewView.OptionsData.Appending := True;
  ALayoutCustomizationForm.PreviewView.OptionsData.Deleting := True;
  ALayoutCustomizationForm.PreviewView.OptionsData.Editing := True;
  ALayoutCustomizationForm.PreviewView.OptionsData.Inserting := True;
end;

function TcxGridWizardLayoutViewHelper.CanSaveData: Boolean;
begin
  Result := True;
end;

function TcxGridWizardLayoutViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardLayoutViewPages);
end;

function TcxGridWizardLayoutViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardLayoutViewPages[Index]
end;

{ TcxGridWizardDBLayoutViewHelper }

procedure TcxGridWizardDBLayoutViewHelper.DeleteItem(const AFieldName: string);
begin
  GridView.GetItemByFieldName(AFieldName).Free;
end;

class function TcxGridWizardDBLayoutViewHelper.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBLayoutView;
end;

function TcxGridWizardDBLayoutViewHelper.GetItemIndexByFieldName(const AFieldName: string): Integer;
var
  AItem: TcxGridDBLayoutViewItem;
begin
  AItem := GridView.GetItemByFieldName(AFieldName);
  if AItem <> nil then
    Result := AItem.Index
  else
    Result := -1;
end;

function TcxGridWizardDBLayoutViewHelper.GetItemCaption(Index: Integer): string;
begin
  Result := inherited GetItemCaption(Index);
  if Result = '' then
    Result := ItemFieldName[Index];
end;

function TcxGridWizardDBLayoutViewHelper.GetPageClassCount: Integer;
begin
  Result := Length(cxGridWizardDBLayoutViewPages);
end;

function TcxGridWizardDBLayoutViewHelper.GetPageClasses(Index: Integer): TClass;
begin
  Result := cxGridWizardDBLayoutViewPages[Index];
end;

function TcxGridWizardDBLayoutViewHelper.GetDataController: TcxDBDataController;
begin
  Result := GridView.DataController;
end;

function TcxGridWizardDBLayoutViewHelper.GetItemFieldName(Index: Integer): string;
begin
  Result := Items[Index].DataBinding.FieldName;
end;

procedure TcxGridWizardDBLayoutViewHelper.SetItemFieldName(Index: Integer; const AValue: string);
begin
  Items[Index].DataBinding.FieldName := AValue;
end;

function TcxGridWizardDBLayoutViewHelper.GetGridView: TcxGridDBLayoutView;
begin
  Result := TcxGridDBLayoutView(inherited GridView);
end;

function TcxGridWizardDBLayoutViewHelper.GetItem(Index: Integer): TcxGridDBLayoutViewItem;
begin
  Result := GridView.Items[Index];
end;

initialization
  cxGridWizardHelperInfoList.Add(TcxGridWizardLayoutViewHelper, HInstance);
  cxGridWizardHelperInfoList.Add(TcxGridWizardDBLayoutViewHelper, HInstance);
end.

