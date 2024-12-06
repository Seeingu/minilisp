
// cc ./main.c -lminilisp -L./zig-out/lib/
extern void run(char *s, int n);

int main()
{
    char s[] = "(+ 1 1)";
    run(s, sizeof(s) - 1);
    return 0;
}