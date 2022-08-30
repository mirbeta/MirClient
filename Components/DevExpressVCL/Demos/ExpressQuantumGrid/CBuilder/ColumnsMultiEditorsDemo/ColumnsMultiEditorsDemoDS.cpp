#include "cxCustomData.hpp"
#include "cxGridTableView.hpp"
#include "ColumnsMultiEditorsDemoDS.h"

const int NameCount = 5;
const String Names[NameCount]  = {"Jerry Campbell", "Ryan Fischer",
                "Tom Hamlett", "Steve Lee", "Jeffrey McClain"};
const String Skills[SkillCount] = {"Programming experiments (in years)",
                "Primary Language", "Secondary Language", "Communication",
                "Start working from", "Custom Information"};

__fastcall TSkillDataSource::TSkillDataSource(TcxGridTableView* ATableView, int ACommunicationLevelCount, int ALanguagesCount)
{
  FTableView = ATableView;
  FCommunicationLevelCount = ACommunicationLevelCount;
  FLanguagesCount = ALanguagesCount;
  int Bounds[2] = {0, NameCount * SkillCount - 1};
  Grades = VarArrayCreate(Bounds, 1, varVariant);
  Randomize();
  for(int i=0; i < NameCount; i++) {
    Grades.PutElement(1 + random(10), i * SkillCount);
    Grades.PutElement(random(FLanguagesCount - 1), i * SkillCount + 1);
    Grades.PutElement(random(FLanguagesCount - 1), i * SkillCount + 2);
    while (Grades.GetElement(i * SkillCount + 1) == Grades.GetElement(i * SkillCount + 2))
      Grades.PutElement(random(FLanguagesCount - 1), i * SkillCount + 2);
    Grades.PutElement(random(FCommunicationLevelCount - 1), i * SkillCount + 3);
    Grades.PutElement(EncodeDate((short)(1992 + random(10)), (short)(1 + random(12)), (short)(1 + random(27))), i * SkillCount + 4);
    Grades.PutElement("Put additional information here", i * SkillCount + 5);
  }
}

int __fastcall TSkillDataSource::GetRecordCount(void)
{
  return(NameCount * SkillCount);
}

void __fastcall TSkillDataSource::SetValue(void * ARecordHandle, void * AItemHandle, const Variant &AValue)
{
  if (FTableView->Columns[(Integer)AItemHandle]->DataBinding->Item->ID == 2)
    Grades.PutElement(AValue, (Integer)ARecordHandle);
}

Variant __fastcall TSkillDataSource::GetValue(void * ARecordHandle, void * AItemHandle)
{
  int AColumnId = FTableView->Columns[Integer(AItemHandle)]->DataBinding->Item->ID;
  switch (AColumnId){
    case 0: return (Names[div((Integer)ARecordHandle, SkillCount).quot]);
    case 1: return (Skills[(Integer)ARecordHandle - (div(Integer(ARecordHandle), SkillCount).quot) * SkillCount]);
    case 2: return (Grades.GetElement((Integer)ARecordHandle));
    default: return(NULL); 
  }
}

