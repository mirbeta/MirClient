{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxGridPopupMenuConsts;

{$I cxVer.inc}

interface

uses
  cxClasses, cxCustomData, dxCore;

resourcestring
  cxSGridNone = 'None';

  // Header popup menu item captions

  cxSGridSortColumnAsc = 'Sort Ascending';
  cxSGridSortColumnDesc = 'Sort Descending';
  cxSGridClearSorting = 'Clear Sorting';
  cxSGridGroupByThisField = 'Group By This Field';
  cxSGridRemoveThisGroupItem = 'Remove from grouping';
  cxSGridGroupByBox = 'Group By Box';
  cxSGridAlignmentSubMenu = 'Alignment';
  cxSGridAlignLeft = 'Align Left';
  cxSGridAlignRight = 'Align Right';
  cxSGridAlignCenter = 'Align Center';
  cxSGridRemoveColumn = 'Remove This Column';
  cxSGridFieldChooser = 'Field Chooser';
  cxSGridBestFit = 'Best Fit';
  cxSGridBestFitAllColumns = 'Best Fit (all columns)';
  cxSGridShowFooter = 'Footer';
  cxSGridShowGroupFooter = 'Group Footers';
  cxSGridShowFindPanel = 'Find Panel';
  cxSGridFullExpand = 'Full Expand';
  cxSGridFullCollapse = 'Full Collapse';
  cxSGridClearGrouping = 'Clear Grouping';
  cxSGridHideGroupByBox = 'Hide Group By Box';

  // Footer popup menu item captions

  cxSGridNoneMenuItem = 'None';
  cxSGridSumMenuItem = 'Sum';
  cxSGridMinMenuItem = 'Min';
  cxSGridMaxMenuItem = 'Max';
  cxSGridCountMenuItem = 'Count';
  cxSGridAvgMenuItem = 'Average';

  // Group row popup menu item captions

  cxSGridSortByGroupValues = 'Sort by Group Value';
  cxSGridSortBySummaryCaption = 'Sort by Group Summary:';
  cxSGridSortBySummary = '%s for %s';

function GetSummaryName(ASummaryKind: TcxSummaryKind): TcxResourceStringID;

implementation

function GetSummaryName(ASummaryKind: TcxSummaryKind): TcxResourceStringID;
const
  SummaryNames: array[TcxSummaryKind] of TcxResourceStringID = (
    @cxSGridNoneMenuItem,
    @cxSGridSumMenuItem,
    @cxSGridMinMenuItem,
    @cxSGridMaxMenuItem,
    @cxSGridCountMenuItem,
    @cxSGridAvgMenuItem);
begin
  Result := SummaryNames[ASummaryKind];
end;

procedure AddcxGridPopupMenuResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('cxSGridNone', @cxSGridNone);
  InternalAdd('cxSGridSortColumnAsc', @cxSGridSortColumnAsc);
  InternalAdd('cxSGridSortColumnDesc', @cxSGridSortColumnDesc);
  InternalAdd('cxSGridClearSorting', @cxSGridClearSorting);
  InternalAdd('cxSGridGroupByThisField', @cxSGridGroupByThisField);
  InternalAdd('cxSGridRemoveThisGroupItem', @cxSGridRemoveThisGroupItem);
  InternalAdd('cxSGridGroupByBox', @cxSGridGroupByBox);
  InternalAdd('cxSGridAlignmentSubMenu', @cxSGridAlignmentSubMenu);
  InternalAdd('cxSGridAlignLeft', @cxSGridAlignLeft);
  InternalAdd('cxSGridAlignRight', @cxSGridAlignRight);
  InternalAdd('cxSGridAlignCenter', @cxSGridAlignCenter);
  InternalAdd('cxSGridRemoveColumn', @cxSGridRemoveColumn);
  InternalAdd('cxSGridFieldChooser', @cxSGridFieldChooser);
  InternalAdd('cxSGridBestFit', @cxSGridBestFit);
  InternalAdd('cxSGridBestFitAllColumns', @cxSGridBestFitAllColumns);
  InternalAdd('cxSGridShowFooter', @cxSGridShowFooter);
  InternalAdd('cxSGridShowGroupFooter', @cxSGridShowGroupFooter);
  InternalAdd('cxSGridShowFindPanel', @cxSGridShowFindPanel);
  InternalAdd('cxSGridFullExpand', @cxSGridFullExpand);
  InternalAdd('cxSGridFullCollapse', @cxSGridFullCollapse);
  InternalAdd('cxSGridClearGrouping', @cxSGridClearGrouping);
  InternalAdd('cxSGridHideGroupByBox', @cxSGridHideGroupByBox);
  InternalAdd('cxSGridNoneMenuItem', @cxSGridNoneMenuItem);
  InternalAdd('cxSGridSumMenuItem', @cxSGridSumMenuItem);
  InternalAdd('cxSGridMinMenuItem', @cxSGridMinMenuItem);
  InternalAdd('cxSGridMaxMenuItem', @cxSGridMaxMenuItem);
  InternalAdd('cxSGridCountMenuItem', @cxSGridCountMenuItem);
  InternalAdd('cxSGridAvgMenuItem', @cxSGridAvgMenuItem);
  InternalAdd('cxSGridSortByGroupValues', @cxSGridSortByGroupValues);
  InternalAdd('cxSGridSortBySummaryCaption', @cxSGridSortBySummaryCaption);
  InternalAdd('cxSGridSortBySummary', @cxSGridSortBySummary);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('cxGridPopupMenu', @AddcxGridPopupMenuResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('cxGridPopupMenu');

end.
