{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express OrgChart                                         }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSORGCHART AND ALL ACCOMPANYING  }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxorgchrstrs;

{$I cxVer.inc}

interface

resourcestring
  sdxOrgChartEditorCancelButton = 'Cancel';
  sdxOrgChartEditorCaption = 'TdxOrgChart Items Editor';
  sdxOrgChartEditorItems = 'Items';
  sdxOrgChartEditorProperties = 'Item Properties';

  sdxOrgChartEditorHintAntialiasing = 'Antialiasing on/off';
  sdxOrgChartEditorHintApplyForAllChildren = 'Set properties for all children of selected item';
  sdxOrgChartEditorHintDeleteItem = 'Delete item';
  sdxOrgChartEditorHintInsertItem = 'Insert new item';
  sdxOrgChartEditorHintInsertSubItem = 'Insert new subitem';
  sdxOrgChartEditorHintRotate =  '90¡ã rotate on/off';
  sdxOrgChartEditorHintZoom = 'Zoom on/off';

  sdxOrgChartEditorChildAlign = 'Child&Align';
  sdxOrgChartEditorColor = '&Color';
  sdxOrgChartEditorHeight = '&Height';
  sdxOrgChartEditorImageAlign = 'ImageAlign';
  sdxOrgChartEditorImageIndex = 'ImageIndex';
  sdxOrgChartEditorShape = '&Shape';
  sdxOrgChartEditorText = 'Text';
  sdxOrgChartEditorWidth = '&Width';

implementation

uses
  dxCore;

procedure AddOrgChartResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxOrgChartEditorCancelButton', @sdxOrgChartEditorCancelButton);
  AProduct.Add('sdxOrgChartEditorCaption', @sdxOrgChartEditorCaption);
  AProduct.Add('sdxOrgChartEditorItems', @sdxOrgChartEditorItems);
  AProduct.Add('sdxOrgChartEditorProperties', @sdxOrgChartEditorProperties);

  AProduct.Add('sdxOrgChartEditorHintAntialiasing', @sdxOrgChartEditorHintAntialiasing);
  AProduct.Add('sdxOrgChartEditorHintApplyForAllChildren', @sdxOrgChartEditorHintApplyForAllChildren);
  AProduct.Add('sdxOrgChartEditorHintDeleteItem', @sdxOrgChartEditorHintDeleteItem);
  AProduct.Add('sdxOrgChartEditorHintInsertItem', @sdxOrgChartEditorHintInsertItem);
  AProduct.Add('sdxOrgChartEditorHintInsertSubItem', @sdxOrgChartEditorHintInsertSubItem);
  AProduct.Add('sdxOrgChartEditorHintRotate', @sdxOrgChartEditorHintRotate);
  AProduct.Add('sdxOrgChartEditorHintZoom', @sdxOrgChartEditorHintZoom);

  AProduct.Add('sdxOrgChartEditorChildAlign', @sdxOrgChartEditorChildAlign);
  AProduct.Add('sdxOrgChartEditorColor', @sdxOrgChartEditorColor);
  AProduct.Add('sdxOrgChartEditorHeight', @sdxOrgChartEditorHeight);
  AProduct.Add('sdxOrgChartEditorImageAlign', @sdxOrgChartEditorImageAlign);
  AProduct.Add('sdxOrgChartEditorImageIndex', @sdxOrgChartEditorImageIndex);
  AProduct.Add('sdxOrgChartEditorShape', @sdxOrgChartEditorShape);
  AProduct.Add('sdxOrgChartEditorText', @sdxOrgChartEditorText);
  AProduct.Add('sdxOrgChartEditorWidth', @sdxOrgChartEditorWidth);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressOrgChart', @AddOrgChartResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressOrgChart');
end.

