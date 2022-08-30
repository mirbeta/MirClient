{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPageControl                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPAGECONTROL AND ALL            }
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

unit cxPCConsts;

{$I cxVer.inc}

interface

resourcestring
  scxPCImageListIndexError = 'Index (%d) must be between 0 and %d';
  scxPCNoBaseImages = 'BaseImages is not assigned';
  scxPCNoRegisteredStyles = 'There are no styles registered';
  scxPCPageIndexError = '%d is an invalid PageIndex value.  PageIndex must be ' +
    'between 0 and %d';
  scxPCPainterClassError = 'PCPainterClass is nil';
  scxPCStandardStyleError = '%s is an unsupported standard style';
  scxPCStyleNameError = '%s is an unregistered style name';
  scxPCTabCountEqualsZero = 'Tabs.Count = 0';
  scxPCTabIndexError = 'Tab''s index (%d) out of bounds';
  scxPCTabVisibleIndexOutsOfBounds = 'TabVisibleIndex (%d) must be between 0 and %d';
  scxPCVisibleTabListEmpty = 'There are no visible tabs';
  scxPCAllowRotateError = '%s style does not support rotation of tabs';
  // default hints
  scxPCDefaultHintCloseButton = 'Close active tab';
  scxPCDefaultHintGoDialogButton = 'Open go dialog';
  scxPCDefaultHintTopLeftButton = 'Previous tab';
  scxPCDefaultHintBottomRightButton = 'Next tab';
  scxPCDefaultHintTabCloseButton = 'Close tab';
  scxPCDefaultHintNewButton = 'New tab';

implementation

uses
  dxCore;

procedure AddExpressPageControlResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAdress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAdress);
  end;

begin
  InternalAdd('scxPCImageListIndexError', @scxPCImageListIndexError);
  InternalAdd('scxPCNoBaseImages', @scxPCNoBaseImages);
  InternalAdd('scxPCNoRegisteredStyles', @scxPCNoRegisteredStyles);
  InternalAdd('scxPCPageIndexError', @scxPCPageIndexError);
  InternalAdd('scxPCPainterClassError', @scxPCPainterClassError);
  InternalAdd('scxPCStandardStyleError', @scxPCStandardStyleError);
  InternalAdd('scxPCStyleNameError', @scxPCStyleNameError);
  InternalAdd('scxPCTabCountEqualsZero', @scxPCTabCountEqualsZero);
  InternalAdd('scxPCTabIndexError', @scxPCTabIndexError);
  InternalAdd('scxPCTabVisibleIndexOutsOfBounds', @scxPCTabVisibleIndexOutsOfBounds);
  InternalAdd('scxPCVisibleTabListEmpty', @scxPCVisibleTabListEmpty);
  InternalAdd('scxPCAllowRotateError', @scxPCAllowRotateError);
  InternalAdd('scxPCDefaultHintCloseButton', @scxPCDefaultHintCloseButton);
  InternalAdd('scxPCDefaultHintGoDialogButton', @scxPCDefaultHintGoDialogButton);
  InternalAdd('scxPCDefaultHintTopLeftButton', @scxPCDefaultHintTopLeftButton);
  InternalAdd('scxPCDefaultHintBottomRightButton', @scxPCDefaultHintBottomRightButton);
  InternalAdd('scxPCDefaultHintTabCloseButton', @scxPCDefaultHintTabCloseButton);
  InternalAdd('scxPCDefaultHintNewButton', @scxPCDefaultHintNewButton);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressPageControl', @AddExpressPageControlResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressPageControl');

end.
