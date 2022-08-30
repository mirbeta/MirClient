{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl common routines                     }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
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

unit dxLayoutStrs;

{$I cxVer.inc}

interface

resourcestring

  sdxLayoutControlContainerCannotBeControl = 'Container cannot be a control for its item.';
  sdxLayoutControlControlIsUsed = 'The %s control is already used by %s item.';

  sdxLayoutControlNewGroupCaption = 'New Group';
  sdxLayoutControlRoot = 'Root';
  sdxLayoutControlNewItemCaption = 'New Item';
  sdxLayoutControlNewEmptySpaceItemCaption = 'Empty Space Item';
  sdxLayoutControlNewSeparatorItemCaption = 'Separator';
  sdxLayoutControlNewLabeledItemCaption = 'Label';
  sdxLayoutControlNewImageItemCaption = 'Image';
  sdxLayoutControlNewSplitterItemCaption = 'Splitter';
  sdxLayoutControlNewHiddenGroup = 'Hidden Group';
  sdxLayoutControlNewAutoCreatedGroup = 'Auto-Created Group';
  sdxLayoutControlEmptyCaption = '<empty>';

  sdxLayoutControlCustomizeFormCaption = 'Customize';

  sdxLayoutControlCustomizeFormAddAuxiliaryItem = 'Add Auxiliary Item';
  sdxLayoutControlCustomizeFormAddGroup = 'Add Group';
  sdxLayoutControlCustomizeFormAddItem = 'Add Item';
  sdxLayoutControlCustomizeFormAddEmptySpaceItem = 'Add Empty Space Item';
  sdxLayoutControlCustomizeFormAddSeparatorItem = 'Add Separator Item';
  sdxLayoutControlCustomizeFormAddSplitterItem = 'Add Splitter Item';
  sdxLayoutControlCustomizeFormAddLabeledItem = 'Add Label Item';
  sdxLayoutControlCustomizeFormAddImageItem = 'Add Image Item';

  sdxLayoutControlCustomizeFormDelete = 'Delete';
  sdxLayoutControlCustomizeFormDeleteHint = 'Delete (Del)';
  sdxLayoutControlCustomizeFormAlignBy = 'Align by';
  sdxLayoutControlCustomizeFormClose = '&Close';
  sdxLayoutControlCustomizeFormExpandAll = 'Expand All';
  sdxLayoutControlCustomizeFormCollapseAll = 'Collapse All';
  sdxLayoutControlCustomizeFormAlignLeftSide = 'Left Side';
  sdxLayoutControlCustomizeFormAlignRightSide = 'Right Side';
  sdxLayoutControlCustomizeFormAlignTopSide = 'Top Side';
  sdxLayoutControlCustomizeFormAlignBottomSide = 'Bottom Side';
  sdxLayoutControlCustomizeFormAlignNone = 'None';
  sdxLayoutControlCustomizeFormTreeViewGroup = '&Layout Tree View';
  sdxLayoutControlCustomizeFormListViewGroup = '&Available Items';
  sdxLayoutControlCustomizeFormTabbedView = '&Tabbed View';
  sdxLayoutControlCustomizeFormTreeView = 'View as Tree / Plain List';
  sdxLayoutControlCustomizeFormStore = 'Store Layout';
  sdxLayoutControlCustomizeFormRestore = 'Restore Layout';
  sdxLayoutControlCustomizeFormRename = 'Rename';
  sdxLayoutControlCustomizeFormUndo = 'Undo';
  sdxLayoutControlCustomizeFormRedo = 'Redo';

  sdxLayoutControlCustomizeFormHAlign = 'Horizontal Alignment';
  sdxLayoutControlCustomizeFormHAlignLeft = 'Left';
  sdxLayoutControlCustomizeFormHAlignCenter = 'Center';
  sdxLayoutControlCustomizeFormHAlignRight = 'Right';
  sdxLayoutControlCustomizeFormHAlignClient = 'Client';
  sdxLayoutControlCustomizeFormHAlignParent = 'Parent Managed';

  sdxLayoutControlCustomizeFormVAlign = 'Vertical Alignment';
  sdxLayoutControlCustomizeFormVAlignTop = 'Top';
  sdxLayoutControlCustomizeFormVAlignCenter = 'Center';
  sdxLayoutControlCustomizeFormVAlignBottom = 'Bottom';
  sdxLayoutControlCustomizeFormVAlignClient = 'Client';
  sdxLayoutControlCustomizeFormVAlignParent = 'Parent Managed';

  sdxLayoutControlCustomizeFormDirection = 'Layout Direction';
  sdxLayoutControlCustomizeFormDirectionHorizontal = 'Horizontal';
  sdxLayoutControlCustomizeFormDirectionVertical = 'Vertical';
  sdxLayoutControlCustomizeFormDirectionTabbed = 'Tabbed';

  sdxLayoutControlCustomizeFormTextPosition = 'Caption Position';
  sdxLayoutControlCustomizeFormTextPositionLeft = 'Left';
  sdxLayoutControlCustomizeFormTextPositionTop = 'Top';
  sdxLayoutControlCustomizeFormTextPositionRight = 'Right';
  sdxLayoutControlCustomizeFormTextPositionBottom = 'Bottom';

  sdxLayoutControlCustomizeFormCaptionAlignHorz = 'Caption Horizontal Alignment';
  sdxLayoutControlCustomizeFormCaptionAlignHorzLeft = 'Left';
  sdxLayoutControlCustomizeFormCaptionAlignHorzCenter = 'Center';
  sdxLayoutControlCustomizeFormCaptionAlignHorzRight = 'Right';

  sdxLayoutControlCustomizeFormCaptionAlignVert = 'Caption Vertical Alignment';
  sdxLayoutControlCustomizeFormCaptionAlignVertTop = 'Top';
  sdxLayoutControlCustomizeFormCaptionAlignVertCenter = 'Center';
  sdxLayoutControlCustomizeFormCaptionAlignVertBottom = 'Bottom';

  sdxLayoutControlCustomizeFormSplitterCollapsible = 'Collapsible';

  sdxLayoutControlCustomizeFormItemCaption = 'Caption';
  sdxLayoutControlCustomizeFormGroupBorder = 'Border';
  sdxLayoutControlCustomizeFormGroupExpandButton = 'Expand Button';

  sdxLayoutControlCustomizeFormGroup = 'Group';
  sdxLayoutControlCustomizeFormUngroup = 'Ungroup';

  sdxLayoutControlEditFormOK = 'OK';
  sdxLayoutControlEditFormCancel = 'Cancel';

  sdxLayoutControlCollapseButtonHint = 'Click to expand';
  sdxLayoutControlExpandButtonHint = 'Click to collapse';
  sdxLayoutControlHomeButtonHint = 'Stop Float';

  // Dialogs
  sdxLayoutControlDesignerCaptionFormat = '%s - Designer';

  // Old version
  sdxLayoutControlCustomizeFormShowBorder = 'Show Border';

implementation

uses
  dxCore;

procedure AddLayoutControlResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('sdxLayoutControlContainerCannotBeControl', @sdxLayoutControlContainerCannotBeControl);
  InternalAdd('sdxLayoutControlControlIsUsed', @sdxLayoutControlControlIsUsed);
  InternalAdd('sdxLayoutControlNewGroupCaption', @sdxLayoutControlNewGroupCaption);
  InternalAdd('sdxLayoutControlRoot', @sdxLayoutControlRoot);
  InternalAdd('sdxLayoutControlNewItemCaption', @sdxLayoutControlNewItemCaption);
  InternalAdd('sdxLayoutControlNewEmptySpaceItemCaption', @sdxLayoutControlNewEmptySpaceItemCaption);
  InternalAdd('sdxLayoutControlNewSeparatorItemCaption', @sdxLayoutControlNewSeparatorItemCaption);
  InternalAdd('sdxLayoutControlNewLabeledItemCaption', @sdxLayoutControlNewLabeledItemCaption);
  InternalAdd('sdxLayoutControlNewImageItemCaption', @sdxLayoutControlNewImageItemCaption);
  InternalAdd('sdxLayoutControlNewSplitterItemCaption', @sdxLayoutControlNewSplitterItemCaption);
  InternalAdd('sdxLayoutControlNewHiddenGroup', @sdxLayoutControlNewHiddenGroup);
  InternalAdd('sdxLayoutControlNewAutoCreatedGroup', @sdxLayoutControlNewAutoCreatedGroup);
  InternalAdd('sdxLayoutControlEmptyCaption', @sdxLayoutControlEmptyCaption);
  InternalAdd('sdxLayoutControlCustomizeFormCaption', @sdxLayoutControlCustomizeFormCaption);
  InternalAdd('sdxLayoutControlCustomizeFormAddAuxiliaryItem', @sdxLayoutControlCustomizeFormAddAuxiliaryItem);
  InternalAdd('sdxLayoutControlCustomizeFormAddGroup', @sdxLayoutControlCustomizeFormAddGroup);
  InternalAdd('sdxLayoutControlCustomizeFormAddItem', @sdxLayoutControlCustomizeFormAddItem);
  InternalAdd('sdxLayoutControlCustomizeFormAddEmptySpaceItem', @sdxLayoutControlCustomizeFormAddEmptySpaceItem);
  InternalAdd('sdxLayoutControlCustomizeFormAddSeparatorItem', @sdxLayoutControlCustomizeFormAddSeparatorItem);
  InternalAdd('sdxLayoutControlCustomizeFormAddSplitterItem', @sdxLayoutControlCustomizeFormAddSplitterItem);
  InternalAdd('sdxLayoutControlCustomizeFormAddLabeledItem', @sdxLayoutControlCustomizeFormAddLabeledItem);
  InternalAdd('sdxLayoutControlCustomizeFormAddImageItem', @sdxLayoutControlCustomizeFormAddImageItem);
  InternalAdd('sdxLayoutControlCustomizeFormDelete', @sdxLayoutControlCustomizeFormDelete);
  InternalAdd('sdxLayoutControlCustomizeFormDeleteHint', @sdxLayoutControlCustomizeFormDeleteHint);
  InternalAdd('sdxLayoutControlCustomizeFormAlignBy', @sdxLayoutControlCustomizeFormAlignBy);
  InternalAdd('sdxLayoutControlCustomizeFormClose', @sdxLayoutControlCustomizeFormClose);
  InternalAdd('sdxLayoutControlCustomizeFormExpandAll', @sdxLayoutControlCustomizeFormExpandAll);
  InternalAdd('sdxLayoutControlCustomizeFormCollapseAll', @sdxLayoutControlCustomizeFormCollapseAll);
  InternalAdd('sdxLayoutControlCustomizeFormAlignLeftSide', @sdxLayoutControlCustomizeFormAlignLeftSide);
  InternalAdd('sdxLayoutControlCustomizeFormAlignRightSide', @sdxLayoutControlCustomizeFormAlignRightSide);
  InternalAdd('sdxLayoutControlCustomizeFormAlignTopSide', @sdxLayoutControlCustomizeFormAlignTopSide);
  InternalAdd('sdxLayoutControlCustomizeFormAlignBottomSide', @sdxLayoutControlCustomizeFormAlignBottomSide);
  InternalAdd('sdxLayoutControlCustomizeFormAlignNone', @sdxLayoutControlCustomizeFormAlignNone);
  InternalAdd('sdxLayoutControlCustomizeFormTreeViewGroup', @sdxLayoutControlCustomizeFormTreeViewGroup);
  InternalAdd('sdxLayoutControlCustomizeFormListViewGroup', @sdxLayoutControlCustomizeFormListViewGroup);
  InternalAdd('sdxLayoutControlCustomizeFormTabbedView', @sdxLayoutControlCustomizeFormTabbedView);
  InternalAdd('sdxLayoutControlCustomizeFormTreeView', @sdxLayoutControlCustomizeFormTreeView);
  InternalAdd('sdxLayoutControlCustomizeFormStore', @sdxLayoutControlCustomizeFormStore);
  InternalAdd('sdxLayoutControlCustomizeFormRestore', @sdxLayoutControlCustomizeFormRestore);
  InternalAdd('sdxLayoutControlCustomizeFormRename', @sdxLayoutControlCustomizeFormRename);
  InternalAdd('sdxLayoutControlCustomizeFormUndo', @sdxLayoutControlCustomizeFormUndo);
  InternalAdd('sdxLayoutControlCustomizeFormRedo', @sdxLayoutControlCustomizeFormRedo);
  InternalAdd('sdxLayoutControlCustomizeFormHAlign', @sdxLayoutControlCustomizeFormHAlign);
  InternalAdd('sdxLayoutControlCustomizeFormHAlignLeft', @sdxLayoutControlCustomizeFormHAlignLeft);
  InternalAdd('sdxLayoutControlCustomizeFormHAlignCenter', @sdxLayoutControlCustomizeFormHAlignCenter);
  InternalAdd('sdxLayoutControlCustomizeFormHAlignRight', @sdxLayoutControlCustomizeFormHAlignRight);
  InternalAdd('sdxLayoutControlCustomizeFormHAlignClient', @sdxLayoutControlCustomizeFormHAlignClient);
  InternalAdd('sdxLayoutControlCustomizeFormHAlignParent', @sdxLayoutControlCustomizeFormHAlignParent);
  InternalAdd('sdxLayoutControlCustomizeFormVAlign', @sdxLayoutControlCustomizeFormVAlign);
  InternalAdd('sdxLayoutControlCustomizeFormVAlignTop', @sdxLayoutControlCustomizeFormVAlignTop);
  InternalAdd('sdxLayoutControlCustomizeFormVAlignCenter', @sdxLayoutControlCustomizeFormVAlignCenter);
  InternalAdd('sdxLayoutControlCustomizeFormVAlignBottom', @sdxLayoutControlCustomizeFormVAlignBottom);
  InternalAdd('sdxLayoutControlCustomizeFormVAlignClient', @sdxLayoutControlCustomizeFormVAlignClient);
  InternalAdd('sdxLayoutControlCustomizeFormVAlignParent', @sdxLayoutControlCustomizeFormVAlignParent);
  InternalAdd('sdxLayoutControlCustomizeFormDirection', @sdxLayoutControlCustomizeFormDirection);
  InternalAdd('sdxLayoutControlCustomizeFormDirectionHorizontal', @sdxLayoutControlCustomizeFormDirectionHorizontal);
  InternalAdd('sdxLayoutControlCustomizeFormDirectionVertical', @sdxLayoutControlCustomizeFormDirectionVertical);
  InternalAdd('sdxLayoutControlCustomizeFormDirectionTabbed', @sdxLayoutControlCustomizeFormDirectionTabbed);
  InternalAdd('sdxLayoutControlCustomizeFormGroupBorder', @sdxLayoutControlCustomizeFormGroupBorder);
  InternalAdd('sdxLayoutControlCustomizeFormGroupExpandButton', @sdxLayoutControlCustomizeFormGroupExpandButton);
  InternalAdd('sdxLayoutControlEditFormOK', @sdxLayoutControlEditFormOK);
  InternalAdd('sdxLayoutControlEditFormCancel', @sdxLayoutControlEditFormCancel);
  InternalAdd('sdxLayoutControlDesignerCaptionFormat', @sdxLayoutControlDesignerCaptionFormat);
  InternalAdd('sdxLayoutControlCustomizeFormTextPosition', @sdxLayoutControlCustomizeFormTextPosition);
  InternalAdd('sdxLayoutControlCustomizeFormTextPositionLeft', @sdxLayoutControlCustomizeFormTextPositionLeft);
  InternalAdd('sdxLayoutControlCustomizeFormTextPositionTop', @sdxLayoutControlCustomizeFormTextPositionTop);
  InternalAdd('sdxLayoutControlCustomizeFormTextPositionRight', @sdxLayoutControlCustomizeFormTextPositionRight);
  InternalAdd('sdxLayoutControlCustomizeFormTextPositionBottom', @sdxLayoutControlCustomizeFormTextPositionBottom);
  InternalAdd('sdxLayoutControlCustomizeFormCaptionAlignHorz', @sdxLayoutControlCustomizeFormCaptionAlignHorz);
  InternalAdd('sdxLayoutControlCustomizeFormCaptionAlignHorzLeft', @sdxLayoutControlCustomizeFormCaptionAlignHorzLeft);
  InternalAdd('sdxLayoutControlCustomizeFormCaptionAlignHorzCenter', @sdxLayoutControlCustomizeFormCaptionAlignHorzCenter);
  InternalAdd('sdxLayoutControlCustomizeFormCaptionAlignHorzRight', @sdxLayoutControlCustomizeFormCaptionAlignHorzRight);
  InternalAdd('sdxLayoutControlCustomizeFormCaptionAlignVert', @sdxLayoutControlCustomizeFormCaptionAlignVert);
  InternalAdd('sdxLayoutControlCustomizeFormCaptionAlignVertTop', @sdxLayoutControlCustomizeFormCaptionAlignVertTop);
  InternalAdd('sdxLayoutControlCustomizeFormCaptionAlignVertCenter', @sdxLayoutControlCustomizeFormCaptionAlignVertCenter);
  InternalAdd('sdxLayoutControlCustomizeFormCaptionAlignVertBottom', @sdxLayoutControlCustomizeFormCaptionAlignVertBottom);
  InternalAdd('sdxLayoutControlCustomizeFormItemCaption', @sdxLayoutControlCustomizeFormItemCaption);
  InternalAdd('sdxLayoutControlCustomizeFormGroup', @sdxLayoutControlCustomizeFormGroup);
  InternalAdd('sdxLayoutControlCustomizeFormUngroup', @sdxLayoutControlCustomizeFormUngroup);
  InternalAdd('sdxLayoutControlCollapseButtonHint', @sdxLayoutControlCollapseButtonHint);
  InternalAdd('sdxLayoutControlExpandButtonHint', @sdxLayoutControlExpandButtonHint);
  InternalAdd('sdxLayoutControlHomeButtonHint', @sdxLayoutControlHomeButtonHint);
  InternalAdd('sdxLayoutControlCustomizeFormSplitterCollapsible', @sdxLayoutControlCustomizeFormSplitterCollapsible);

  // Old version
  InternalAdd('sdxLayoutControlCustomizeFormShowBorder', @sdxLayoutControlCustomizeFormShowBorder);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressLayoutControl', @AddLayoutControlResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressLayoutControl');

end.
