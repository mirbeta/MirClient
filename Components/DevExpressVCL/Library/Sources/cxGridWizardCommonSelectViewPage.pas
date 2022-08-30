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

unit cxGridWizardCommonSelectViewPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, DB, cxGraphics, cxControls,
  cxClasses, cxLookAndFeels, cxLookAndFeelPainters, ComCtrls, dxLayoutContainer, dxLayoutControlAdapters, StdCtrls,
  cxRadioGroup, cxContainer, cxEdit, cxListView, dxLayoutControl, cxGrid, cxGridCustomView, dxWizardControl, ImgList,
  cxGridWizardCustomPage, cxGridWizardStrs, dxLayoutLookAndFeels, dxGalleryControl;

type
  { cxGridWizardCommonSelectViewPageFrame }

  TcxGridWizardCommonSelectViewPageFrame = class(TcxGridWizardCustomPageFrame)
    ilGridViewImages: TcxImageList;
    lciGridViews: TdxLayoutItem;
    lvGridViews: TdxGalleryControl;
    procedure lvGridViewsDblClick(Sender: TObject);
    procedure lvGridViewsItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
  protected
    function GetCanFinishWizard: Boolean; override;
    function GetCanJumpToNextPage: Boolean; override;
    function GetCanJumpToPrevPage: Boolean; override;
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;
    procedure PopulateGridViews;
  public
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

uses
  cxGridWizardCustomHelper, dxGallery, dxCore;

{$R *.dfm}

const
  GroupNames: array [TcxGridWizardGridViewType] of Pointer = (
    @scxgwSelectViewPageGroupHeaderUnboundViews,
    @scxgwSelectViewPageGroupHeaderDBViews,
    @scxgwSelectViewPageGroupHeaderServerModeViews
  );

type
  TdxGalleryItemAccess = class(TdxGalleryItem);

{ cxGridWizardCommonSelectViewPageFrame }

procedure TcxGridWizardCommonSelectViewPageFrame.ApplySettings;
var
  ACheckedItem: TdxGalleryItem;
  AGridViewClass: TcxCustomGridViewClass;
begin
  ACheckedItem := lvGridViews.Gallery.GetCheckedItem;
  if ACheckedItem <> nil then
  begin
    AGridViewClass := TcxCustomGridViewClass(ACheckedItem.Tag);
    if (Helper = nil) or (AGridViewClass <> Helper.GetGridViewClass) then
    begin
      CreateHelperForGridView(AGridViewClass);
      Helper.SaveGridViewOptionsData;
      CreatePagesContent;
    end;
  end;
end;

procedure TcxGridWizardCommonSelectViewPageFrame.LoadSettings;
var
  AItem: TdxGalleryItem;
begin
  PopulateGridViews;
  lvGridViews.Gallery.UncheckAll;
  if Helper <> nil then
  begin
    AItem := lvGridViews.Gallery.FindItemByTag(TdxNativeInt(Helper.GetGridViewClass));
    if (AItem <> nil) and TdxGalleryItemAccess(AItem).Group.Visible then
      AItem.Checked := True;
  end;
end;

function TcxGridWizardCommonSelectViewPageFrame.GetCanFinishWizard: Boolean;
begin
  Result := False;
end;

function TcxGridWizardCommonSelectViewPageFrame.GetCanJumpToNextPage: Boolean;
begin
  Result := lvGridViews.Gallery.GetCheckedItem <> nil;
end;

function TcxGridWizardCommonSelectViewPageFrame.GetCanJumpToPrevPage: Boolean;
begin
  Result := not (IsMultiLevelStructure and IsDetailViewCustomizing);
end;

function TcxGridWizardCommonSelectViewPageFrame.GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwSelectViewPageDescription);
end;

function TcxGridWizardCommonSelectViewPageFrame.GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwSelectViewPageTitle);
end;

procedure TcxGridWizardCommonSelectViewPageFrame.PopulateGridViews;
var
  AGroups: array[TcxGridWizardGridViewType] of TdxGalleryControlGroup;
  AHelperInfo: TcxGridWizardHelperInfo;
  AIndex: TcxGridWizardGridViewType;
  AItem: TdxGalleryControlItem;
  I: Integer;
begin
  ilGridViewImages.Clear;
  lvGridViews.BeginUpdate;
  try
    lvGridViews.Gallery.Groups.Clear;

    for AIndex := Low(TcxGridWizardGridViewType) to High(TcxGridWizardGridViewType) do
    begin
      AGroups[AIndex] := lvGridViews.Gallery.Groups.Add;
      AGroups[AIndex].Caption := cxGetResourceString(GroupNames[AIndex]);
    end;

    for I := 0 to HelperInfoList.Count - 1 do
    begin
      AHelperInfo := HelperInfoList[I];
      if not IsMultiLevelStructure or IsDetailViewCustomizing or AHelperInfo.HelperClass.CanBeMasterView then
      begin
        AItem := AGroups[cxGridWizardGetGridViewGroup(AHelperInfo.GridViewClass)].Items.Add;
        AItem.Caption := cxGridRegisteredViews.GetDescriptionByClass(AHelperInfo.GridViewClass);
        AItem.Tag := TdxNativeInt(AHelperInfo.GridViewClass);
        AItem.ImageIndex := ilGridViewImages.Add(AHelperInfo.Glyph, nil);
      end;
    end;

    for AIndex := Low(TcxGridWizardGridViewType) to High(TcxGridWizardGridViewType) do
      AGroups[AIndex].Visible := (AGroups[AIndex].ItemCount > 0) and (not IsMultiLevelStructure or (AIndex = gvtDB));
  finally
    lvGridViews.EndUpdate;
  end;
end;

{ Events }

procedure TcxGridWizardCommonSelectViewPageFrame.lvGridViewsDblClick(Sender: TObject);
begin
  if CanJumpToNextPage then
    JumpToNextPage;
end;

procedure TcxGridWizardCommonSelectViewPageFrame.lvGridViewsItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
begin
  UpdateOwnerButtonsState;
end;

end.
