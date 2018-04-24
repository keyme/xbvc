#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

#include <xbvc_core.h>
#include <cobs.h>

extern int xbvc_encode_get_command(struct x_get_command *src, uint8_t *dest,
                                   int max_len);
extern int xbvc_decode_get_command(uint8_t *src, struct x_get_command *dest,
                                   int max_len);
extern int xbvc_encode_get_response(struct x_get_response *src, uint8_t *dest,
                                    int max_len);
extern int xbvc_decode_get_response(uint8_t *src, struct x_get_response *dest,
                                    int max_len);

void xbvc_handle_get_command(struct x_get_command *msg)
{
}

void xbvc_handle_get_response(struct x_get_response *msg)
{
}

void xbvc_handle_heartbeat(struct x_heartbeat *msg)
{
}

void print_get_response(struct x_get_response *msg)
{
    printf("x_get_response:\n");
    printf("Error: 0x%x\n", msg->Error);
    printf("Target: 0x%x\n", msg->Target);
    printf("Index: 0x%x\n", msg->Index);
    printf("Foo: 0x%x\n", msg->Foo);
    printf("Result: 0x%x\n", msg->Result);
    printf("Bar: 0x%x\n", msg->Bar);
    printf("-------------------\n");

}

void test_encode_round_trip(void)
{
    struct x_get_response msg = {0};
    struct x_get_response decoded_msg = {0};
    uint8_t encode_buf[128];

    msg.Error = 0xdeadbeef;
    msg.Target = 0xF00;
    msg.Index = 0xcafe;
    msg.Foo = 0xa5;
    msg.Result = 0xbadd00d;
    msg.Bar = 0xff;
    print_get_response(&msg);

    int enc_len = xbvc_encode_get_response(&msg, encode_buf,
                                           sizeof(encode_buf));
    int dec_len = xbvc_decode_get_response(encode_buf, &decoded_msg,
                                           sizeof(encode_buf));

    printf("%d | %d\n", enc_len, dec_len);

    printf("[");
    for (int i = 0; i < enc_len; i++) {
        printf("0x%x", encode_buf[i]);
        if (i < enc_len - 1) {
            printf(", ");
        }
    }
    printf("]");
    printf("\n");

    print_get_response(&decoded_msg);

    assert(memcmp(&msg, &decoded_msg, sizeof(struct x_get_response)) == 0);
}

int main()
{
    test_encode_round_trip();
    printf("Hello world!\n");
    return 0;
}
