{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDocking                                           }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDOCKING AND ALL ACCOMPANYING   }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxDockConsts;

{$I cxVer.inc}

interface

uses
  dxCore;

const
  dxDefaultImageHeight = 16;
  dxDefaultImageWidth = 16;
  dxDockZonesWidth = 20;
  dxResizeZonesWidth = 3;
  dxSelectionFrameWidth = 4;
  dxAutoHideInterval = 500;
  dxAutoHideMovingInterval = 1;
  dxAutoHideMovingSize = 20;
  dxAutoShowInterval = 300;
  dxTabsScrollInterval = 300;

resourcestring
  sdxInvalidLayoutSiteDeleting = 'You cannot delete a TdxLayoutDockSite.';
  sdxInvalidFloatSiteDeleting = 'You cannot delete a TdxFloatDockSite.';
  sdxInvalidFloatingDeleting = 'You cannot delete a TdxCustomDockSite in floating mode.';
  sdxInvalidParentAssigning = 'You cannot set the parent for this component.';
  sdxInvalidOwner = 'The Owner of the TdxCustomDockControl must be TCustomForm.';
  sdxInvalidParent = 'The Parent of the %s must be TdxCustomDockControl.';
  sdxInvalidDockSiteParent = 'The Parent of the TdxDockSite cannot be TdxCustomDockControl.';
  sdxInvalidFloatSiteParent = 'The Parent of the TdxFloatDockSite can only be TdxFloatForm.';
  sdxInvalidPanelChild = 'You cannot insert a TdxCustomDockControl into TdxDockPanel (%s is being inserted).';
  sdxInvalidSiteChild = 'You can only insert a TdxCustomDockControl into TdxCustomDockSite (%s is being inserted).';
  sdxInvaldZoneOwner = 'You cannot create a TdxZone without the owning TdxCustomDockControl.';
  sdxInternalErrorAutoHide = 'Internal error while autohide controls.';
  sdxInternalErrorPainter = 'Internal error in the TdxCustomDockControl painter.';
  sdxInternalErrorLayout = 'Internal error in the %s object layout.';
  sdxInternalErrorCreateLayout = 'Internal error while creating a %s object layout.';
  sdxInternalErrorDestroyLayout = 'Internal error while destroying a %s object layout.';
  sdxManagerError = 'You cannot have more than one TdxDockingManager instance on a Form.';
  sdxAncestorError = 'Docking and auto hiding operations are not available for controls declared in an ancestor form.';

const
  sdxManagerOwnerError = 'Redundant TdxDockingManager component declaration found.' + dxCRLF +
    'You can safely remove the TdxDockingManager component from "%s" (%s) because only TCustomForm descendants can be effective owners of TdxDockingManager components.';

implementation

procedure AddExpressDockingResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('sdxInvalidLayoutSiteDeleting', @sdxInvalidLayoutSiteDeleting);
  InternalAdd('sdxInvalidFloatSiteDeleting', @sdxInvalidFloatSiteDeleting);
  InternalAdd('sdxInvalidFloatingDeleting', @sdxInvalidFloatingDeleting);
  InternalAdd('sdxInvalidParentAssigning', @sdxInvalidParentAssigning);
  InternalAdd('sdxInvalidOwner', @sdxInvalidOwner);
  InternalAdd('sdxInvalidParent', @sdxInvalidParent);
  InternalAdd('sdxInvalidDockSiteParent', @sdxInvalidDockSiteParent);
  InternalAdd('sdxInvalidFloatSiteParent', @sdxInvalidFloatSiteParent);
  InternalAdd('sdxInvalidPanelChild', @sdxInvalidPanelChild);
  InternalAdd('sdxInvalidSiteChild', @sdxInvalidSiteChild);
  InternalAdd('sdxInvaldZoneOwner', @sdxInvaldZoneOwner);
  InternalAdd('sdxInternalErrorAutoHide', @sdxInternalErrorAutoHide);
  InternalAdd('sdxInternalErrorPainter', @sdxInternalErrorPainter);
  InternalAdd('sdxInternalErrorLayout', @sdxInternalErrorLayout);
  InternalAdd('sdxInternalErrorCreateLayout', @sdxInternalErrorCreateLayout);
  InternalAdd('sdxInternalErrorDestroyLayout', @sdxInternalErrorDestroyLayout);
  InternalAdd('sdxManagerError', @sdxManagerError);
  InternalAdd('sdxAncestorError', @sdxAncestorError);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressDocking', @AddExpressDockingResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressDocking');

end.
