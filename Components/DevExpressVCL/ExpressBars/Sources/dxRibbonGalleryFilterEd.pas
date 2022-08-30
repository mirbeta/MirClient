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

unit dxRibbonGalleryFilterEd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Menus, cxEdit, CheckLst,
  dxRibbonGallery, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxCheckListBox,
  cxButtons, dxLayoutContainer, dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses, dxLayoutControl;

type
  TfmGalleryFilterGroups = class(TForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    clbGroups: TcxCheckListBox;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
  public
    procedure Apply(AFilterCategory: TdxRibbonGalleryFilterCategory);
    procedure Init(AFilterCategory: TdxRibbonGalleryFilterCategory);
  end;

implementation

{$R *.dfm}

{ TfmGalleryFilterGroups }

procedure TfmGalleryFilterGroups.Apply(AFilterCategory: TdxRibbonGalleryFilterCategory);
var
  I: Integer;
begin
  AFilterCategory.Groups.Clear;
  for I := 0 to clbGroups.Items.Count - 1 do
  begin
    if clbGroups.Items[I].Checked then
      AFilterCategory.Groups.Add(TdxRibbonGalleryGroup(clbGroups.Items[I].ItemObject));
  end;
end;

procedure TfmGalleryFilterGroups.Init(AFilterCategory: TdxRibbonGalleryFilterCategory);
var
  AGroup: TdxRibbonGalleryGroup;
  AItem: TcxCheckListBoxItem;
  I: Integer;
begin
  Caption := Format('Editing %s.GalleryFilter.Categories[%d].Groups', [AFilterCategory.GalleryItem.Name, AFilterCategory.Index]);
  clbGroups.Clear;
  for I := 0 to AFilterCategory.GalleryItem.GalleryGroups.Count - 1 do
  begin
    AGroup := AFilterCategory.GalleryItem.GalleryGroups[I];
    AItem := clbGroups.Items.Add;
    AItem.Text := Format('%d - "%s"', [I, AGroup.Header.Caption]);
    AItem.ItemObject := AGroup;
    AItem.Checked := AFilterCategory.Groups.IndexOf(AGroup) >= 0;
  end;
end;

end.
