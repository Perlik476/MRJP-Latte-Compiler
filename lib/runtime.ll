; Declare the printf function
declare i32 @printf(i8*, ...)
declare i32 @scanf(i8*, ...)
declare void @exit(i32)

; Create a constant global string for the printf format specifier
@format = constant [3 x i8] c"%d\00"
@format2 = constant [2 x i8] c"\0A\00"

define void @fun.printInt(i32 %x) {
entry:
  ; Call printf with the format specifier and the integer
  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @format, i32 0, i32 0), i32 %x)
  ; Add a newline
  call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @format2, i32 0, i32 0))

  ret void
}

define i32 @fun.readInt() {
  ; Call scanf with the format specifier and the integer
  %x = alloca i32
  call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @format, i32 0, i32 0), i32* %x)
  %y = load i32, i32* %x
  ret i32 %y
}

define void @fun.error() {
  call void @exit(i32 1)
  ret void
}