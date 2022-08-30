{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library graphics classes          }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxActionsReg;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Classes, Graphics, ActnList, dxActions, cxGraphics, cxControls, dxUIGenerator,
{$IFDEF DELPHIXE3}
  Actions, ActionEditors,
{$ENDIF}
  ImgList;

{$IFDEF DELPHIXE3}
type

{$IFDEF DELPHIXE8}
  TdxIDEActionsImageLink = TObject;
{$ELSE}
  TdxIDEActionsImageLink = TChangeLink;
{$ENDIF}

  { TdxIDEActions }

  TdxIDEActions = class(TIDEActions)
  public
    class var OldIDEActionsClass: TIDEActionsClass;

    class procedure AssignAction(Source, Destination: TBasicAction); override;
    class function BaseActionClass: TContainedActionClass; override;
    class function BaseActionListClass: TContainedActionListClass; override;
    class procedure CopyImageIfAvailable(const NewAction: TContainedAction; const ActionList: TContainedActionList); override;
    class function CreateImageList(ActionList: TContainedActionList): TCustomImageList; override;
    class function DefaultActionClass: TContainedActionClass; override;
    class procedure RegisterImageLink(const ActionList: TContainedActionList; const ImageLink: TdxIDEActionsImageLink); override;
    class procedure UnregisterImageLink(const ActionList: TContainedActionList; const ImageLink: TdxIDEActionsImageLink); override;
  end;

{$ENDIF}

implementation

type
  TdxBasicActionAccess = class(TdxBasicAction);

{$IFDEF DELPHIXE3}

{ TdxIDEActions }

class procedure TdxIDEActions.AssignAction(Source, Destination: TBasicAction);
var
  ABitmap: TcxBitmap32;
  ADestination: TdxBasicActionAccess;
  AImages: TcxImageList;
  ASource: TdxBasicAction;
begin
  OldIDEActionsClass.AssignAction(Source, Destination);

  if (Source is TdxBasicAction) and (Destination is TdxBasicAction) then
  begin
    ASource := TdxBasicActionAccess(Source);
    if not (ASource.Images is TcxImageList) then
      Exit;
    ADestination := TdxBasicActionAccess(Destination);
  end
  else
    Exit;

  AImages := TcxImageList(ASource.Images);
  if IsImageAssigned(AImages, ASource.ImageIndex) then
  begin
    FreeAndNil(ADestination.FImage);
    FreeAndNil(ADestination.FMask);

    ABitmap := TcxBitmap32.CreateSize(16, 16);
    AImages.GetImage(ASource.ImageIndex, ABitmap);
    ADestination.FImage := ABitmap;

  end;
end;

class function TdxIDEActions.BaseActionClass: TContainedActionClass;
begin
  Result := OldIDEActionsClass.BaseActionClass;
end;

class function TdxIDEActions.BaseActionListClass: TContainedActionListClass;
begin
  Result := OldIDEActionsClass.BaseActionListClass;
end;

class procedure TdxIDEActions.CopyImageIfAvailable(const NewAction: TContainedAction; const ActionList: TContainedActionList);
var
  AAction: TdxBasicActionAccess;
  AImages: TCustomImageList;
begin
  if (NewAction is TdxBasicAction) and (ActionList is TActionList) then
  begin
    AAction := TdxBasicActionAccess(NewAction);
    AImages := TActionList(ActionList).Images;
    if AAction.FImage = nil then
      AAction.ImageIndex := TdxUIGeneratorHelper.AddImage(AImages,
        AAction.FDefaultImageNameInIconLibrary, TdxUIGeneratorAdapter.DefaultImageSet)
    else
      if (AAction.FImage is TcxBitmap32) and (AImages is TcxImageList) then
      try
        AAction.ImageIndex := TcxImageList(AImages).Add(TBitmap(AAction.FImage), nil)
      finally
        FreeAndNil(AAction.FImage);
        FreeAndNil(AAction.FMask);
      end
      else
        OldIDEActionsClass.CopyImageIfAvailable(NewAction, ActionList);
  end
  else
    OldIDEActionsClass.CopyImageIfAvailable(NewAction, ActionList);
end;

class function TdxIDEActions.CreateImageList(ActionList: TContainedActionList): TCustomImageList;
begin
  Result := OldIDEActionsClass.CreateImageList(ActionList);
end;

class function TdxIDEActions.DefaultActionClass: TContainedActionClass;
begin
  Result := OldIDEActionsClass.DefaultActionClass;
end;

class procedure TdxIDEActions.RegisterImageLink(
  const ActionList: TContainedActionList; const ImageLink: TdxIDEActionsImageLink);
begin
  OldIDEActionsClass.RegisterImageLink(ActionList, ImageLink);
end;

class procedure TdxIDEActions.UnregisterImageLink(
  const ActionList: TContainedActionList; const ImageLink: TdxIDEActionsImageLink);
begin
  OldIDEActionsClass.UnregisterImageLink(ActionList, ImageLink);
end;

initialization
  if TdxIDEActions.OldIDEActionsClass = nil then
  begin
    TdxIDEActions.OldIDEActionsClass := GetIDEActions('VCL');
    if (TdxIDEActions.OldIDEActionsClass <> nil) and (TdxIDEActions.OldIDEActionsClass <> TdxIDEActions) then
    begin
      UnregisterActionsInFramework('VCL');
      RegisterActionsInFramework('VCL', TdxIDEActions);
    end
    else
      TdxIDEActions.OldIDEActionsClass := nil;
  end;

finalization
  if TdxIDEActions.OldIDEActionsClass <> nil then
  begin
    UnregisterActionsInFramework('VCL');
    RegisterActionsInFramework('VCL', TdxIDEActions.OldIDEActionsClass);
    TdxIDEActions.OldIDEActionsClass := nil;
  end;
{$ENDIF}

end.
