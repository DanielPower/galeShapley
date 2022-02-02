M = {}

n = int(input())
k = int(input())
input()
hospitalPreferenceLists = []
for _ in range(k):
    hospitalPreferenceLists.append([int(i) for i in input().split(" ")])
input()
studentPreferenceLists = []
for _ in range(k):
    studentPreferenceLists.append([int(i) for i in input().split(" ")])
input()
hospitals = [hospitalPreferenceLists[int(i)] for i in input().split(" ")]
input()
students = [studentPreferenceLists[int(i)] for i in input().split(" ")]

unmatchedHospitals = [i for i in range(len(hospitals))]
hospitalMap = {}
studentMap = {}

print(n, k)
print(hospitals)
print(students)

while len(unmatchedHospitals):
    print("Iteration")
    print(unmatchedHospitals)
    hospitalId = unmatchedHospitals.pop()
    hospitalPreferences = hospitals[hospitalId]
    studentId = hospitalPreferences.pop()
    studentPreferences = students[studentId]
    studentCurrentHospitalId = studentMap.get(studentId)

    print((hospitalId, (studentId, studentCurrentHospitalId)))

    if studentCurrentHospitalId is None:
        print("a")
        studentMap[studentId] = hospitalId
        hospitalMap[hospitalId] = studentId
    elif studentPreferences.index(hospitalId) < studentPreferences.index(
        studentCurrentHospitalId
    ):
        print("b")
        studentMap[studentId] = hospitalId
        hospitalMap[studentCurrentHospitalId] = None
        hospitalMap[hospitalId] = studentId
        unmatchedHospitals.append(studentCurrentHospitalId)

    print((hospitalMap.get(hospitalId), studentMap.get(studentId)))

print(hospitalMap)
print(studentMap)
