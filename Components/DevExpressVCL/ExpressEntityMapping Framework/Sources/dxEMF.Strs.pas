{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
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

unit dxEMF.Strs;

{$I cxVer.Inc}

interface

const

  sdxSessionDifferentObjectsWithSameKeyException = 'Objects with duplicate keys found.';
  sdxSessionEnteringTheX0StateFromTheX1StateIsProhibit = 'Entering state ''%s'' from state ''%s'' is prohibited ' +
    'due to state ''%s'' for ''%s''.';
  sdxSessionMostProbablyYouAreTryingToInitiateAnObjectEx = 'Most probably, you are trying to initiate an object ' +
    'loading operation while the previous object loading operation is not yet completed.';
  sdxSessionMostProbablyYouAreTryingToInitiateAnObject = 'Most probably, you are trying to initiate an object ' +
    'loading operation while the previous object saving operation is not yet completed.';
  sdxSessionTranSequenceBegin = 'Session cannot start more than one transaction.';
  sdxSessionTranSequenceRollback = 'Session cannot roll back a transaction because no transaction is currently active.';
  sdxSessionCannotDeletedNotAssociatedObject = 'Object ''%0:s'' cannot be deleted by removing it from the ''%1:s'' object''s ' +
    'collection because object ''%1:s'' is not associated with a session.';

  sdxMetadataAssociationInvalidAssociatedMemberNotFound = 'Malformed association ''%s''. ' +
    'The associated member for ''%s.%s'' cannot be found in class ''%s''.';
  sdxMetadataAssociationNotCreated = 'Class ''%s'' does not initialize association ''%s''. ' +
    'To initialize it, create a collection using the TdxEMFCollections.Create<> class function in the class''s constructor and assign this collection to the associated field.';
  sdxMetadataAssociationInvalidAssociationAttributeOnlyForCollectionOrReference = 'The Association attribute cannot be used ' +
    'for member ''%s.%s''. A reference or collection is expected.';
  sdxMetadataAssociationInvalidNonPersistentClassInTheAssociation = 'The Association attribute ' +
    'cannot be used for member ''%s.%s'' because class ''%s'' is not persistent.';
  sdxMetadataAssociationInvalidPropertyTypeMismatch = 'Association ''%0:s'' between classes ''%1:s'' and ''%3:s'' is invalid. ' +
    'Class ''%1:s'' contains property ''%2:s'' which refers to class ''%3:s''. Class ''%3:s'' contains property ''%4:s'', ' +
    'which inappropriately refers to class ''%5:s'' instead of class ''%1:s''.';
  sdxMetadataAssociationInvalidMoreThanOneAssociatedMemberFound = 'Association ''%s'' is ambiguous between the members ''%s'' and ''%s'' of class ''%s''.';
  sdxMetadataAssociationInvalidNoAssociationCollectionInAssociation = 'Malformed association ''%s'' between ''%s.%s'' and ' +
    '''%s.%s''. A collection is expected at one side of the association.';
  sdxMetadataCannotResolveEntityInfo = 'Metadata cannot be resolved for type ''%s''.';
  sdxMetadataIncorrectPathMemberDoesNotExist = 'Property path ''%s'' is incorrect for type ''%s'' because member ''%s.%s'' does not exist.';
  sdxMetadataKeyPropertyDoesNotExist = 'A primary key column is not specified for type ''%s''.';
  sdxMetadataBaseEntityNotFound = 'No ancestor of class ''%s'' is declared as an entity.';
  sdxMetadataBaseEntityNoDiscriminatorColumn = 'The ''%s'' class''s ancestor does not declare a discriminator column.';
  sdxMetadataCannotResolveUnnamedAssociation = 'An unnamed association that relates to entities ''%s'' cannot be resolved in entity ''%s''. Specify the association''s name.';
  sdxMetadataFieldNotFound = 'Field ''%s'' cannot be found in class ''%s''.';
  sdxMetadataPropertyNotFound = 'Property ''%s'' cannot be found in class ''%s''.';
  sdxMetadataFieldPropertyNotFound = 'Field/property ''%s'' cannot be found in class ''%s''.';
  sdxMetadataFieldPropertyNotPersistent = 'Field/property ''%s'' is neither persistent nor marked with the Association attribute in class ''%s''.';
  sdxMetadataDuplicateInheritedMemberNameFound = 'Duplicate member name ''%s'' is found in class ''%s'' (inherited from class ''%s'')';

  sdxCollectionsNotCollectionProperty = '''%s'' is not a collection property. It should be marked with the Association attribute and derive its type from IdxEMFCollection.';
  sdxCollectionsRecurringObjectAdd = 'Recurring add.';
  sdxCollectionsWantNotDeleteFilteredAggregateCollection = 'Deleting object ''%s'' with a partially loaded aggregated collection ''%s'' may cause orphan objects/records.';

  sdxAutoIncrementedKeyNotSupported = 'The ''%s'' type''s auto-incremented key is not supported for ''%s''.';
  sdxTableNotFound = 'Table ''%s'' cannot be found.';

  sdxIncorrectPathNonReferenceMember = 'Path ''%1:s'' is incorrect for type ''%0:s'' because ''%0:s.%1:s'' is not a reference member.';
  sdxInvalidPropertyPathException = 'Path ''%s'' is incorrect for type ''%s''.';
  sdxCommonMethodOrOperationNotImplemented = 'The method or operation is not implemented.';
  sdxPersistentAliasExpanderReferenceOrCollectionExpectedInTheMiddleOfThePath =
    'A path includes an incorrect member ''%s''. A reference member or collection association is expected.';

  sdxFilteringTheIifFunctionOperatorRequiresThree = 'The Iif function operator requires three or more arguments. ' +
    'The number of arguments must be odd.';

  sdxGeneratorOneOfBinaryOperatorsOperandsIsNull = 'One of BinaryOperator''s operands is null.';
  sdxGeneratorTheUseOfATopLevelSingleAggregateIsProhibited = 'The use of a top level Single aggregate is prohibited.';
  sdxGeneratorTheUseOfNestedSingleAggregatesIsProhibited = 'The use of nested Single aggregates is prohibited.';


  sdxSessionInternalEMFError = 'Internal framework error.';
  sdxSessionMissingConnection = 'The DataProvider property is not specified by component ''%s''.';


  sdxArgumentCountInformationNotFound = 'Argument count information cannot be found for function type ''%s''.';
  sdxExpressionNotRegistered = 'Expression not registered: %s.';
  sdxFunctionInfoNotFound = 'FunctionInfo cannot be found for function type ''%s''.';
  sdxFunctionInfoNotFound2 = 'FunctionInfo cannot be found for function type ''%s'' with %d arguments.';
  sdxUnknownCategory = 'Unknown category';

  sdxCannotSkipRecords = 'Records cannot be skipped without the ORDER BY clause.';
  sdxClassIsNotEntity = 'Class ''%s'' is not an entity.';
  sdxColumnNotFound = 'Column ''%s'' cannot be found in table ''%s''.';
  sdxConnectionProviderUnableToCreateDBObject = 'Unable to create ''%s'' ''%s''. Parent: ''%s''. Error: %s.';
  sdxMissingProviderSQL = 'Missing SQL connection provider.';
  sdxMissingSession = 'Missing session.';
  sdxPrimaryKeyNotFoundInBaseClass = 'Primary key cannot be found in the base class.';
  sdxStatementNotFinished = 'Statement is not finished.';
  sdxUnknownModificationStatement = 'Unknown modification statement.';
  sdxUnknownProviderSQL = 'Unknown provider SQL: %s.';

  sdxFilteringExceptionsTextSingleCriterionExpected = 'A single criterion is expected.';
  sdxFilteringExceptionsTextInvalidTypeCode = 'invalid type code';
  sdxFilteringExceptionsTextSyntaxError = 'syntax error';
  sdxFilteringExceptionsTextIrrecoverableSyntaxError = 'Irrecoverable syntax error';
  sdxFilteringExceptionsTextIrrecoverableSyntaxErrorAtEnd = 'irrecoverable syntax error at the end of file';
  sdxFilteringExceptionsTextLexerInvalidInputCharacter = 'Invalid input character ''%s''';
  sdxFilteringExceptionsTextLexerNonClosedElement = 'Malformed %s: missing closing ''%s''';
  sdxFilteringExceptionsTextLexerInvalidElement = 'Invalid %s value: ''%s''';
  sdxFilteringExceptionsTextLexerElementPropertyName = 'property name';
  sdxFilteringExceptionsTextLexerElementStringLiteral = 'string literal';
  sdxFilteringExceptionsTextLexerCantRestoreUserObject = 'User object cannot be restored. Tag ''%s'', data ''%s'', Exception: ''%s''.';
  sdxFilteringExceptionsTextLexerWrongFunction = 'Wrong function - ''%s''.';
  sdxFilteringExceptionsTextLexerWrongArgumentCount = 'Wrong argument count (%d). Function - ''%s''.';
  sdxFilteringExceptionsTextLexerElementDateTimeLiteral = 'date/time literal';
  sdxFilteringExceptionsTextLexerElementDateTimeOrUserTypeLiteral = 'date/time or user type literal';
  sdxFilteringExceptionsTextLexerElementGuidLiteral = 'GUID literal';
  sdxFilteringExceptionsTextLexerElementNumberLiteral = 'numeric literal';
  sdxFilteringExceptionsTextGrammarCatchAllErrorMessage = 'Parser error at line %d, character %d: %s; (''%s'').';
  sdxFilteringExceptionsTextErrorPointer = '{FAILED HERE}';
  sdxEvaluatorOperandsCountNotEqualOne = 'Operator.Operands.Count <> 1';
  sdxEvaluatorOperandsCountLessTwo = 'Operator.Operands.Count < 2';

  sdxLinqExpressionFieldNotFound = 'Field ''%s'' cannot be found in entity ''%s''.';
  sdxConnectionIsNull = 'The Connection property is not specified by component ''%s''.';

  sdxAlias = 'sdxAlias';
  sdxEmptyStack = 'sdxEmptyStack';
  sdxEmptySubCriteria = 'sdxEmptySubCriteria';
  sdxErrorMessage = 'sdxErrorMessage';
  sdxExpressionIsNotValidType = 'Expression is not a valid type.';
  sdxEntityNameNotSpecified = 'The EntityName property is not specified by component ''%s''.';
  sdxEntityCannotBeFound = 'Entity ''%s'' specified by component ''%s'' cannot be found.';
  sdxMemberColumn = 'sdxMemberColumn';
  sdxNeedOperandProperty = 'sdxNeedOperandProperty';
  sdxNotImplemented = 'Not implemented';
  sdxOrderByTermOutOfRange = 'The ORDER BY term is out of range - should be between 1 and %d.';
  sdxSaveToStream = 'sdxSaveToStream';
  sdxTableIsDBProjection = 'sdxTableIsDBProjection';
  sdxTheOperatorOperandsCountNot0 = 'AOperator.Operands.Count <> 0';
  sdxTypeMismatch = 'sdxTypeMismatch';

  sdxUpLevelsClassInfos = 'sdxUpLevelsClassInfos';
  sdxValue = 'sdxValue';

implementation

end.
