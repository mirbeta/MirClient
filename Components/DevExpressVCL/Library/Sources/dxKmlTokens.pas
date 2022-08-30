{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxKmlTokens;

interface

{$I cxVer.inc}

const
  SdxKmlKmlOldNamespaceName = 'http://earth.google.com/kml/';
  SdxKmlKmlNamespaceName = 'http://www.opengis.net/kml/';

  SdxKmlKml = 'kml';
  SdxKmlDocument = 'Document';
  SdxKmlFolder = 'Folder';
  SdxKmlId = 'id';

  SdxKmlPlacemark = 'Placemark';
  SdxKmlPoint = 'Point';
  SdxKmlLineString = 'LineString';
  SdxKmlLinearRing = 'LinearRing';
  SdxKmlPolygon = 'Polygon';
  SdxKmlMultiGeometry = 'MultiGeometry';

  SdxKmlIcon = 'Icon';
  SdxKmlName = 'name';
  SdxKmlOuterBoundaryIs = 'outerBoundaryIs';
  SdxKmlInnerBoundaryIs = 'innerBoundaryIs';
  SdxKmlAddress = 'address';
  SdxKmlPhoneNumber = 'phoneNumber';
  SdxKmlDescription = 'description';

  //styles
  SdxKmlStyle = 'Style';
  SdxKmlStyleMap = 'StyleMap';
  SdxKmlStyleUrl = 'styleUrl';
  SdxKmlIconStyle = 'IconStyle';
  SdxKmlLabelStyle = 'LabelStyle';
  SdxKmlLineStyle = 'LineStyle';
  SdxKmlPolyStyle = 'PolyStyle';
  SdxKmlBalloonStyle = 'BalloonStyle';
  SdxKmlListStyle = 'ListStyle';

  SdxKmlPair = 'Pair';
  SdxKmlKey = 'key';
  SdxKmlHotSpot = 'hotSpot';
  SdxKmlXUnits = 'xunits';
  SdxKmlYUnits = 'yunits';
  SdxKmlX = 'x';
  SdxKmlY = 'y';
  SdxKmlScale = 'scale';
  SdxKmlHeading = 'heading';
  SdxKmlHRef = 'href';
  SdxKmlColor = 'color';
  SdxKmlColorMode = 'colorMode';
  SdxKmlWidth = 'width';
  SdxKmlFill = 'fill';
  SdxKmlOutline = 'outline';
  SdxKmlBgColor = 'bgColor';
  SdxKmlTextColor = 'textColor';
  SdxKmlText = 'text';
  SdxKmlDisplayMode = 'displayMode';
  SdxKmlListItemType = 'listItemType';
  SdxKmlItemIcon = 'ItemIcon';
  SdxKmlIconItemMode = 'iconItemMode';


  SdxKmlCoordinates = 'coordinates';

  SdxKmlPixels = 'pixels';
  SdxKmlInsetPixels = 'insetPixels';
  SdxKmlFraction = 'fraction';

  SdxKmlNormal = 'normal';
  SdxKmlRandom = 'random';

  SdxKmlCheck = 'check';
  SdxKmlCheckOffOnly = 'checkOffOnly';
  SdxKmlCheckHideChildren = 'checkHideChildren';
  SdxKmlRadioFolder = 'radioFolder';

  SdxKmlOpen = 'open';
  SdxKmlClosed = 'closed';
  SdxKmlError = 'error';
  SdxKmlFetching0 = 'fetching0';
  SdxKmlFetching1 = 'fetching1';
  SdxKmlFetching2 = 'fetching2';

  SdxKmlNormalStyleState = 'normal';
  SdxKmlHighlightStyleState = 'highlight';

implementation

end.
