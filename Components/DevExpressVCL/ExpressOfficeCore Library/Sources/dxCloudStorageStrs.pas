{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressOfficeCore Library classes                        }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL     }
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

unit dxCloudStorageStrs;

{$I cxVer.inc}

interface

resourcestring
  // Exceptions
  sdxCloudStorageProviderDoesNotSupportThisOperationException = 'The provider does not support this operation';
  sdxCloudStorageRootShouldNotHaveParentException = 'The root should not have a parent';
  sdxCloudStorageSpecialFolderCannotBeDeletedOrMovedException = 'The special folder cannot be deleted or moved';
  sdxCloudStorageRootCannotBeDeletedOrMovedException  = 'The root cannot be deleted or moved';


{$REGION 'Special folders'}
  sdxCloudStorageRecentFolderName = 'Recent';
  sdxCloudStorageSharedWithMeFolderName = 'Shared with me';
  sdxCloudStorageSharedByMeFolderName = 'Shared by me';
  sdxCloudStorageStarredFolderName = 'Starred';
  sdxCloudStorageDiscoverFolderName = 'Discover';
  sdxCloudStorageTrashFolderName = 'Trash';

  // GoogleDrive
  sdxCloudStorageGoogleDriveProviderRecentFolderName = 'Recent';
  sdxCloudStorageGoogleDriveProviderSharedWithMeFolderName = 'Shared with me';
  sdxCloudStorageGoogleDriveProviderStarredFolderName = 'Starred';
  sdxCloudStorageGoogleDriveProviderTrashFolderName = 'Trash';

  // Microsoft OneDrive
  sdxCloudStorageMicrosoftOneDriveProviderTrashFolderName = 'Recycle bin';
  sdxCloudStorageMicrosoftOneDriveProviderDiscoverFolderName = 'Discover';
{$ENDREGION 'Special folders'}

implementation

uses
  dxCore, cxClasses, Graphics;

procedure AddResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxCloudStorageProviderDoesNotSupportThisOperationException', @sdxCloudStorageProviderDoesNotSupportThisOperationException);
  AProduct.Add('sdxCloudStorageRootShouldNotHaveParentException', @sdxCloudStorageRootShouldNotHaveParentException);
  AProduct.Add('sdxCloudStorageSpecialFolderCannotBeDeletedOrMovedException', @sdxCloudStorageSpecialFolderCannotBeDeletedOrMovedException);
  AProduct.Add('sdxCloudStorageRootCannotBeDeletedOrMovedException ', @sdxCloudStorageRootCannotBeDeletedOrMovedException );
  AProduct.Add('sdxCloudStorageRecentFolderName', @sdxCloudStorageRecentFolderName);
  AProduct.Add('sdxCloudStorageSharedWithMeFolderName', @sdxCloudStorageSharedWithMeFolderName);
  AProduct.Add('sdxCloudStorageSharedByMeFolderName', @sdxCloudStorageSharedByMeFolderName);
  AProduct.Add('sdxCloudStorageStarredFolderName', @sdxCloudStorageStarredFolderName);
  AProduct.Add('sdxCloudStorageDiscoverFolderName', @sdxCloudStorageDiscoverFolderName);
  AProduct.Add('sdxCloudStorageTrashFolderName', @sdxCloudStorageTrashFolderName);
  AProduct.Add('sdxCloudStorageGoogleDriveProviderRecentFolderName', @sdxCloudStorageGoogleDriveProviderRecentFolderName);
  AProduct.Add('sdxCloudStorageGoogleDriveProviderSharedWithMeFolderName', @sdxCloudStorageGoogleDriveProviderSharedWithMeFolderName);
  AProduct.Add('sdxCloudStorageGoogleDriveProviderStarredFolderName', @sdxCloudStorageGoogleDriveProviderStarredFolderName);
  AProduct.Add('sdxCloudStorageGoogleDriveProviderTrashFolderName', @sdxCloudStorageGoogleDriveProviderTrashFolderName);
  AProduct.Add('sdxCloudStorageMicrosoftOneDriveProviderTrashFolderName', @sdxCloudStorageMicrosoftOneDriveProviderTrashFolderName);
  AProduct.Add('sdxCloudStorageMicrosoftOneDriveProviderDiscoverFolderName', @sdxCloudStorageMicrosoftOneDriveProviderDiscoverFolderName);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressOfficeCore Library', @AddResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressOfficeCore Library', @AddResourceStringNames);

end.
