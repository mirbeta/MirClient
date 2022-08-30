{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxRibbonGalleryReg;

{$I cxVer.inc}

interface

procedure Register;

implementation

uses
  DesignIntf, DesignEditors,
  SysUtils, Classes, Graphics, ImgList, dxBar, dxCoreReg, cxLibraryReg, dxBarReg,
  dxRibbonGallery, cxPropEditors, Controls, dxRibbonGalleryFilterEd, cxGraphics,
  cxDesignWindows, dxGalleryDesigner;

type

  TdxRibbonGalleryItemAccess = class(TdxRibbonGalleryItem);

  { TdxRibbonGalleryImageIndexProperty }

  TdxRibbonGalleryImageIndexProperty = class(TImageIndexProperty)
  private
    function GetGroup: TdxRibbonGalleryGroup;
  protected
    property Group: TdxRibbonGalleryGroup read GetGroup;
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxRibbonGalleryFilterGroupsProperty }

  TdxRibbonGalleryFilterGroupsProperty = class(TClassProperty)
  protected
    function GetAttributes: TPropertyAttributes; override;
    function FilterCategory: TdxRibbonGalleryFilterCategory;
  public
    procedure Edit; override;
  end;

  { TdxRibbonGalleryGroupAssignedValuesProperty }

  TdxRibbonGalleryGroupAssignedValuesProperty = class(TSetProperty)
  protected
    FProc: TGetPropProc;
    procedure GetPropProc(const Prop: IProperty);
  public
    procedure GetProperties(Proc: TGetPropProc); override;
  end;

  TdxRibbonGalleryGroupAccess = class(TdxRibbonGalleryGroup);

{ TdxRibbonGalleryImageIndexProperty }

function TdxRibbonGalleryImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxRibbonGalleryGroupAccess(Group).Images;
end;

function TdxRibbonGalleryImageIndexProperty.GetGroup: TdxRibbonGalleryGroup;
begin
  Result := (GetComponent(0) as TdxRibbonGalleryGroupItem).Group;
end;

{ TdxRibbonGalleryFilterGroupsProperty }

procedure TdxRibbonGalleryFilterGroupsProperty.Edit;
begin
  with TfmGalleryFilterGroups.Create(nil) do
  try
    Init(FilterCategory);
    if ShowModal = mrOk then
    begin
      Apply(FilterCategory);
      dxBarDesignerModified(FilterCategory.GalleryItem.BarManager);
    end;
  finally
    Free;
  end;
end;

function TdxRibbonGalleryFilterGroupsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paValueEditable];
end;

function TdxRibbonGalleryFilterGroupsProperty.FilterCategory: TdxRibbonGalleryFilterCategory;
begin
  Result := TdxRibbonGalleryFilterCategoryGroups(GetOrdValue).FilterCategory;
end;

{ TdxRibbonGalleryGroupAssignedValuesProperty }

procedure TdxRibbonGalleryGroupAssignedValuesProperty.GetProperties(Proc: TGetPropProc);
begin
  FProc := Proc;
  inherited GetProperties(GetPropProc);
end;

procedure TdxRibbonGalleryGroupAssignedValuesProperty.GetPropProc(const Prop: IProperty);
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
    if SameText(Prop.GetName, 'avSpaceBetweenItems') or
      SameText(Prop.GetName, 'avItemPullHighLighting') then Exit;
  FProc(Prop);
end;

{ TdxRibbonGalleryDesignerPropertyEditor }

var
  FDesigner: TcxDesignFormEditor;

type
  TdxRibbonGalleryDesignerPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure TdxRibbonGalleryDesignerPropertyEditor.Edit;
begin
  FDesigner := ShowFormEditorClass(Designer, GetComponent(0) as TdxRibbonGalleryItem, TfrmGalleryDesigner);
end;

function TdxRibbonGalleryDesignerPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TdxRibbonGalleryDesignerPropertyEditor.GetValue: string;
begin
  Result := Format('(%s)', [TdxRibbonGalleryGroups.ClassName]);
end;

procedure Register;
begin
  RegisterComponents(dxBarProductPage, [TdxRibbonDropDownGallery]);
  RegisterNoIcon([TdxRibbonGalleryItem]);
  RegisterNoIcon([TdxRibbonGalleryGroupItem, TdxRibbonGalleryGroup]);

  RegisterPropertyEditor(TypeInfo(TdxRibbonGalleryGroups), TdxRibbonGalleryItem, 'GalleryCategories',
    TdxRibbonGalleryDesignerPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TBitmap), TdxRibbonGalleryGroupItem,
    'Glyph', TcxBitmapProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxRibbonGalleryGroupItem,
    'ImageIndex', TdxRibbonGalleryImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TdxBarItemLinks), TdxCustomRibbonGalleryItem,
    'ItemLinks', TdxBarItemLinksPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TList), TdxRibbonGalleryFilterCategory,
    'Groups', TdxRibbonGalleryFilterGroupsProperty);
  RegisterPropertyEditor(TypeInfo(TdxRibbonGalleryGroupOptionsAssignedValues),
    TdxRibbonGalleryGroupOptions, 'AssignedValues',
    TdxRibbonGalleryGroupAssignedValuesProperty);

  HideClassProperties(TdxRibbonGalleryGroup, ['Name', 'Tag']);
  HideClassProperties(TdxRibbonGalleryGroupItem, ['Name']);

  HideClassProperties(TdxRibbonGalleryOptions, ['CanCollapse', 'Collapsed',
    'ItemSizeInRibbon', 'MinColumnCount', 'ItemPullHighlighting', 'ItemSize', 'ItemTextKind',
    'EqualItemSizeInAllGroups', 'RowCount', 'ShowScrollBar', 'SubMenuResizing', 'ShowItemHint']);
  HideClassProperties(TdxRibbonGalleryGroupOptions, ['ItemPullHighlighting']);
end;

end.
