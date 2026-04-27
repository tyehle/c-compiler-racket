int main(void) {
    int a = 7;
    if(a)
        goto exit;

    a += 13;
    return a;

    exit: return 1;
}
